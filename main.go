package main

import (
	"bytes"
	"embed"
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"io/fs"
	"log"
	"math"
	"math/rand"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	text_template "text/template"
	"time"
)

//go:embed templates/*.html templates/partials/*.html templates/report/*.tmpl static/*
var embeddedFS embed.FS

// Category represents a comparison category
type Category struct {
	ID     string `json:"id"`
	Label  string `json:"label"`
	Prompt string `json:"prompt"`
}

// Default categories
var DefaultCategories = []Category{
	{ID: "general", Label: "General", Prompt: "Which ring do you prefer?"},
	{ID: "cut", Label: "Cut", Prompt: "Which cut do you prefer?"},
	{ID: "color", Label: "Color", Prompt: "Which color do you prefer?"},
	{ID: "setting", Label: "Setting", Prompt: "Which setting do you prefer?"},
	{ID: "style", Label: "Style", Prompt: "Which style do you prefer?"},
}

// Glicko-2 constants
const (
	InitialRating     = 1500.0 // Starting rating (Glicko-2 scale)
	InitialRD         = 350.0  // Starting rating deviation (high uncertainty)
	InitialVolatility = 0.06   // Starting volatility
	Tau               = 0.5    // System constant (controls volatility change)
	ConvergenceTol    = 0.000001
	MinRD             = 30.0  // Minimum RD (maximum confidence)
	MaxRD             = 350.0 // Maximum RD (minimum confidence)
)

// Ring represents a single ring image with Glicko-2 ratings
type Ring struct {
	Filename        string             `json:"filename"`
	Scores          map[string]float64 `json:"scores"`           // Glicko-2 rating (mu)
	RD              map[string]float64 `json:"rd"`               // Rating deviation
	Volatility      map[string]float64 `json:"volatility"`       // Rating volatility
	ComparisonCount map[string]int     `json:"comparison_count"`
}

// Comparison represents a single vote
type Comparison struct {
	RingA     string    `json:"ring_a"`
	RingB     string    `json:"ring_b"`
	Winner    string    `json:"winner"` // "ring_a", "ring_b", or "draw" for same/skip
	Timestamp time.Time `json:"timestamp"`
}

// Results stores all voting data
type Results struct {
	Rings       map[string]*Ring        `json:"rings"`
	Comparisons map[string][]Comparison `json:"comparisons"`
	Categories  []Category              `json:"categories"`
}

// App holds the application state
type App struct {
	results   *Results
	templates *template.Template
	mu        sync.RWMutex
	dataPath  string
	ringsDir  string
}

func NewApp() *App {
	app := &App{
		dataPath: "data/results.json",
		ringsDir: "rings",
	}

	// Create required directories if they don't exist
	if err := os.MkdirAll("data", 0755); err != nil {
		log.Printf("Warning: could not create data directory: %v", err)
	}
	if err := os.MkdirAll("rings", 0755); err != nil {
		log.Printf("Warning: could not create rings directory: %v", err)
	}

	app.loadTemplates()
	app.loadOrInitResults()
	return app
}

func (app *App) loadTemplates() {
	funcMap := template.FuncMap{
		"sub": func(a, b int) int { return a - b },
		"add": func(a, b int) int { return a + b },
		"percent": func(score float64) int {
			// Convert Glicko-2 rating to percentage (1500 = 50%, 1700 = 75%, 1300 = 25%)
			return int(math.Min(100, math.Max(0, (score-1100)/8)))
		},
		"rdConfidence": func(rd float64) int {
			// Convert RD to confidence (350 = 0%, 30 = 100%)
			return int(100.0 * (1.0 - (rd-MinRD)/(MaxRD-MinRD)))
		},
		"rdClass": func(rd float64) string {
			conf := 100.0 * (1.0 - (rd-MinRD)/(MaxRD-MinRD))
			if conf >= 70 {
				return "rd-low" // low RD = high confidence = green
			} else if conf >= 40 {
				return "rd-medium"
			}
			return "rd-high" // high RD = low confidence = red
		},
	}

	app.templates = template.Must(template.New("").Funcs(funcMap).ParseFS(embeddedFS, "templates/*.html"))
	template.Must(app.templates.ParseFS(embeddedFS, "templates/partials/*.html"))
}

func (app *App) loadOrInitResults() {
	app.mu.Lock()
	defer app.mu.Unlock()

	// Try to load existing results
	data, err := os.ReadFile(app.dataPath)
	if err == nil {
		var results Results
		if json.Unmarshal(data, &results) == nil {
			app.results = &results
			// Ensure categories exist (migrate old data)
			if len(app.results.Categories) == 0 {
				app.results.Categories = DefaultCategories
			}
			app.syncRings()
			return
		}
	}

	// Initialize fresh results
	app.results = &Results{
		Rings:       make(map[string]*Ring),
		Comparisons: make(map[string][]Comparison),
		Categories:  DefaultCategories,
	}

	// Initialize comparisons map for each category
	for _, cat := range app.results.Categories {
		app.results.Comparisons[cat.ID] = []Comparison{}
	}

	// Scan rings directory
	app.syncRings()
	app.saveResults()
}

// Helper methods for categories
func (app *App) getCategories() []Category {
	return app.results.Categories
}

func (app *App) getCategoryIDs() []string {
	ids := make([]string, len(app.results.Categories))
	for i, cat := range app.results.Categories {
		ids[i] = cat.ID
	}
	return ids
}

func (app *App) getCategoryLabel(id string) string {
	for _, cat := range app.results.Categories {
		if cat.ID == id {
			return cat.Label
		}
	}
	return id
}

func (app *App) getCategoryPrompt(id string) string {
	for _, cat := range app.results.Categories {
		if cat.ID == id {
			return cat.Prompt
		}
	}
	return "Which do you prefer?"
}

func (app *App) getCategoryLabelsMap() map[string]string {
	m := make(map[string]string)
	for _, cat := range app.results.Categories {
		m[cat.ID] = cat.Label
	}
	return m
}

func (app *App) syncRings() {
	files, err := os.ReadDir(app.ringsDir)
	if err != nil {
		log.Printf("Error reading rings directory: %v", err)
		return
	}

	for _, file := range files {
		if file.IsDir() {
			continue
		}
		ext := strings.ToLower(filepath.Ext(file.Name()))
		if ext != ".png" && ext != ".jpg" && ext != ".jpeg" && ext != ".webp" {
			continue
		}

		if ring, exists := app.results.Rings[file.Name()]; !exists {
			// Create new ring with Glicko-2 initial values
			scores := make(map[string]float64)
			rd := make(map[string]float64)
			volatility := make(map[string]float64)
			counts := make(map[string]int)
			for _, cat := range app.getCategoryIDs() {
				scores[cat] = InitialRating
				rd[cat] = InitialRD
				volatility[cat] = InitialVolatility
				counts[cat] = 0
			}
			app.results.Rings[file.Name()] = &Ring{
				Filename:        file.Name(),
				Scores:          scores,
				RD:              rd,
				Volatility:      volatility,
				ComparisonCount: counts,
			}
		} else {
			// Migrate existing rings - add RD and Volatility if missing
			if ring.RD == nil {
				ring.RD = make(map[string]float64)
			}
			if ring.Volatility == nil {
				ring.Volatility = make(map[string]float64)
			}
			for _, cat := range app.getCategoryIDs() {
				if _, ok := ring.RD[cat]; !ok {
					// Estimate RD based on comparison count (more comparisons = lower RD)
					count := ring.ComparisonCount[cat]
					if count > 0 {
						// Reduce RD based on comparisons, but not below MinRD
						ring.RD[cat] = math.Max(MinRD, InitialRD-float64(count)*20)
					} else {
						ring.RD[cat] = InitialRD
					}
				}
				if _, ok := ring.Volatility[cat]; !ok {
					ring.Volatility[cat] = InitialVolatility
				}
				// Migrate old 1000-scale ratings to 1500-scale if needed
				if ring.Scores[cat] < 1200 && ring.Scores[cat] > 800 {
					ring.Scores[cat] = ring.Scores[cat] + 500 // Shift from 1000 to 1500 center
				}
			}
		}
	}
}

func (app *App) saveResults() {
	data, err := json.MarshalIndent(app.results, "", "  ")
	if err != nil {
		log.Printf("Error marshaling results: %v", err)
		return
	}

	if err := os.MkdirAll(filepath.Dir(app.dataPath), 0755); err != nil {
		log.Printf("Error creating data directory: %v", err)
		return
	}

	if err := os.WriteFile(app.dataPath, data, 0644); err != nil {
		log.Printf("Error writing results: %v", err)
	}
}

// Glicko-2 helper functions

// g function - reduces impact of opponent's RD
func g(rd float64) float64 {
	return 1.0 / math.Sqrt(1.0+3.0*rd*rd/(math.Pi*math.Pi))
}

// E function - expected score
func E(mu, muJ, rdJ float64) float64 {
	return 1.0 / (1.0 + math.Exp(-g(rdJ)*(mu-muJ)))
}

// Convert rating to Glicko-2 scale (internal)
func toGlicko2Scale(rating, rd float64) (float64, float64) {
	mu := (rating - 1500.0) / 173.7178
	phi := rd / 173.7178
	return mu, phi
}

// Convert from Glicko-2 scale back to rating
func fromGlicko2Scale(mu, phi float64) (float64, float64) {
	rating := mu*173.7178 + 1500.0
	rd := phi * 173.7178
	return rating, rd
}

// UpdateGlicko2 updates ratings for both players after a match
// outcome: 1.0 = ringA wins, 0.0 = ringB wins, 0.5 = draw
func UpdateGlicko2(
	ratingA, rdA, volA float64,
	ratingB, rdB, volB float64,
	outcome float64,
) (newRatingA, newRdA, newVolA, newRatingB, newRdB, newVolB float64) {

	// Convert to Glicko-2 scale
	muA, phiA := toGlicko2Scale(ratingA, rdA)
	muB, phiB := toGlicko2Scale(ratingB, rdB)

	// Calculate for player A
	gPhiB := g(phiB)
	eA := E(muA, muB, phiB)
	vA := 1.0 / (gPhiB * gPhiB * eA * (1 - eA))

	deltaA := vA * gPhiB * (outcome - eA)

	// Update volatility for A (simplified - using iterative algorithm)
	newVolA = updateVolatility(phiA, vA, deltaA, volA)

	// Update RD for A
	phiStarA := math.Sqrt(phiA*phiA + newVolA*newVolA)
	newPhiA := 1.0 / math.Sqrt(1.0/(phiStarA*phiStarA)+1.0/vA)

	// Update rating for A
	newMuA := muA + newPhiA*newPhiA*gPhiB*(outcome-eA)

	// Calculate for player B (outcome is reversed)
	gPhiA := g(phiA)
	eB := E(muB, muA, phiA)
	vB := 1.0 / (gPhiA * gPhiA * eB * (1 - eB))

	deltaB := vB * gPhiA * ((1 - outcome) - eB)

	// Update volatility for B
	newVolB = updateVolatility(phiB, vB, deltaB, volB)

	// Update RD for B
	phiStarB := math.Sqrt(phiB*phiB + newVolB*newVolB)
	newPhiB := 1.0 / math.Sqrt(1.0/(phiStarB*phiStarB)+1.0/vB)

	// Update rating for B
	newMuB := muB + newPhiB*newPhiB*gPhiA*((1-outcome)-eB)

	// Convert back to rating scale
	newRatingA, newRdA = fromGlicko2Scale(newMuA, newPhiA)
	newRatingB, newRdB = fromGlicko2Scale(newMuB, newPhiB)

	// Clamp RD values
	newRdA = math.Max(MinRD, math.Min(MaxRD, newRdA))
	newRdB = math.Max(MinRD, math.Min(MaxRD, newRdB))

	return
}

// updateVolatility uses the iterative algorithm from Glicko-2
func updateVolatility(phi, v, delta, sigma float64) float64 {
	a := math.Log(sigma * sigma)
	deltaSquared := delta * delta
	phiSquared := phi * phi

	f := func(x float64) float64 {
		expX := math.Exp(x)
		tmp := phiSquared + v + expX
		return (expX*(deltaSquared-phiSquared-v-expX))/(2*tmp*tmp) - (x-a)/(Tau*Tau)
	}

	// Initial bounds
	A := a
	var B float64
	if deltaSquared > phiSquared+v {
		B = math.Log(deltaSquared - phiSquared - v)
	} else {
		k := 1.0
		for f(a-k*Tau) < 0 {
			k++
		}
		B = a - k*Tau
	}

	// Iterative algorithm
	fA := f(A)
	fB := f(B)
	for math.Abs(B-A) > ConvergenceTol {
		C := A + (A-B)*fA/(fB-fA)
		fC := f(C)
		if fC*fB < 0 {
			A = B
			fA = fB
		} else {
			fA = fA / 2
		}
		B = C
		fB = fC
	}

	return math.Exp(A / 2)
}

// Get next matchup for a category - Glicko-2 optimized
func (app *App) getNextMatchup(category string) (string, string, bool) {
	app.mu.RLock()
	defer app.mu.RUnlock()

	if len(app.results.Rings) < 2 {
		return "", "", false
	}

	// Get all ring filenames with their uncertainty (RD)
	type ringInfo struct {
		filename string
		rating   float64
		rd       float64
	}
	var rings []ringInfo
	for filename, ring := range app.results.Rings {
		rings = append(rings, ringInfo{
			filename: filename,
			rating:   ring.Scores[category],
			rd:       ring.RD[category],
		})
	}

	// Sort by RD (highest uncertainty first) - prioritize rings we know least about
	sort.Slice(rings, func(i, j int) bool {
		return rings[i].rd > rings[j].rd
	})

	// Build set of ALL compared pairs to avoid repeats
	comparedPairs := make(map[string]bool)
	comparisons := app.results.Comparisons[category]
	for _, c := range comparisons {
		if c.Winner != "skip" { // Skipped pairs can be shown again
			comparedPairs[c.RingA+"|"+c.RingB] = true
			comparedPairs[c.RingB+"|"+c.RingA] = true
		}
	}

	// Strategy: Pick the highest-uncertainty ring, then find a good opponent
	// Good opponent = similar rating OR also high uncertainty
	for _, ringA := range rings[:min(10, len(rings))] { // Consider top 10 most uncertain
		// Find best opponent for ringA
		var bestOpponent *ringInfo
		bestScore := -1.0

		for i := range rings {
			ringB := &rings[i]
			if ringB.filename == ringA.filename {
				continue
			}

			key := ringA.filename + "|" + ringB.filename
			if comparedPairs[key] {
				continue
			}

			// Score = how informative this matchup would be
			// Higher score for: similar ratings + high uncertainty in either
			ratingDiff := math.Abs(ringA.rating - ringB.rating)
			uncertaintyBonus := (ringA.rd + ringB.rd) / 2

			// Prefer close ratings (ratingDiff near 0) and high uncertainty
			// Score decreases as rating difference increases
			score := uncertaintyBonus - ratingDiff*0.5

			if score > bestScore {
				bestScore = score
				bestOpponent = ringB
			}
		}

		if bestOpponent != nil {
			// Randomly swap order for display
			if rand.Intn(2) == 0 {
				return ringA.filename, bestOpponent.filename, true
			}
			return bestOpponent.filename, ringA.filename, true
		}
	}

	// Fallback: random from high-uncertainty rings
	if len(rings) >= 2 {
		rand.Shuffle(len(rings), func(i, j int) { rings[i], rings[j] = rings[j], rings[i] })
		return rings[0].filename, rings[1].filename, true
	}

	return "", "", false
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// Record a vote using Glicko-2
// winner can be ringA, ringB, "draw" (same), or "skip"
func (app *App) recordVote(category, ringA, ringB, winner string) {
	app.mu.Lock()
	defer app.mu.Unlock()

	ringAData := app.results.Rings[ringA]
	ringBData := app.results.Rings[ringB]

	if ringAData == nil || ringBData == nil {
		return
	}

	// Determine outcome for Glicko-2
	var outcome float64
	switch winner {
	case ringA:
		outcome = 1.0 // ringA wins
	case ringB:
		outcome = 0.0 // ringB wins
	case "draw", "same":
		outcome = 0.5 // Draw
	case "skip":
		// Skip doesn't update ratings, just record the comparison
		app.results.Comparisons[category] = append(app.results.Comparisons[category], Comparison{
			RingA:     ringA,
			RingB:     ringB,
			Winner:    "skip",
			Timestamp: time.Now(),
		})
		app.saveResults()
		return
	default:
		return
	}

	// Update ratings using Glicko-2
	newRatingA, newRdA, newVolA, newRatingB, newRdB, newVolB := UpdateGlicko2(
		ringAData.Scores[category], ringAData.RD[category], ringAData.Volatility[category],
		ringBData.Scores[category], ringBData.RD[category], ringBData.Volatility[category],
		outcome,
	)

	ringAData.Scores[category] = newRatingA
	ringAData.RD[category] = newRdA
	ringAData.Volatility[category] = newVolA
	ringAData.ComparisonCount[category]++

	ringBData.Scores[category] = newRatingB
	ringBData.RD[category] = newRdB
	ringBData.Volatility[category] = newVolB
	ringBData.ComparisonCount[category]++

	// Record comparison
	app.results.Comparisons[category] = append(app.results.Comparisons[category], Comparison{
		RingA:     ringA,
		RingB:     ringB,
		Winner:    winner,
		Timestamp: time.Now(),
	})

	app.saveResults()
}

// Undo last vote in a category - returns the undone pair so user can revote
func (app *App) undoLastVote(category string) (ringA, ringB string, ok bool) {
	app.mu.Lock()
	defer app.mu.Unlock()

	comparisons := app.results.Comparisons[category]
	if len(comparisons) == 0 {
		return "", "", false
	}

	// Get last comparison
	last := comparisons[len(comparisons)-1]

	// Remove it from the list
	app.results.Comparisons[category] = comparisons[:len(comparisons)-1]

	// Recalculate ratings for affected rings by replaying all comparisons
	// This is simpler and more accurate than trying to reverse Glicko-2
	app.recalculateCategory(category)

	log.Printf("Undid comparison: %s vs %s (winner: %s)", last.RingA, last.RingB, last.Winner)

	app.saveResults()
	return last.RingA, last.RingB, true
}

// Recalculate all ratings for a category from scratch
func (app *App) recalculateCategory(category string) {
	// Reset all rings to initial values
	for _, ring := range app.results.Rings {
		ring.Scores[category] = InitialRating
		ring.RD[category] = InitialRD
		ring.Volatility[category] = InitialVolatility
		ring.ComparisonCount[category] = 0
	}

	// Replay all comparisons
	for _, comp := range app.results.Comparisons[category] {
		ringA := app.results.Rings[comp.RingA]
		ringB := app.results.Rings[comp.RingB]

		if ringA == nil || ringB == nil {
			continue
		}

		var outcome float64
		switch comp.Winner {
		case comp.RingA:
			outcome = 1.0
		case comp.RingB:
			outcome = 0.0
		case "draw", "same":
			outcome = 0.5
		case "skip":
			continue // Skips don't affect ratings
		default:
			continue
		}

		newRatingA, newRdA, newVolA, newRatingB, newRdB, newVolB := UpdateGlicko2(
			ringA.Scores[category], ringA.RD[category], ringA.Volatility[category],
			ringB.Scores[category], ringB.RD[category], ringB.Volatility[category],
			outcome,
		)

		ringA.Scores[category] = newRatingA
		ringA.RD[category] = newRdA
		ringA.Volatility[category] = newVolA
		ringA.ComparisonCount[category]++

		ringB.Scores[category] = newRatingB
		ringB.RD[category] = newRdB
		ringB.Volatility[category] = newVolB
		ringB.ComparisonCount[category]++
	}
}

// Reset a category
func (app *App) resetCategory(category string) {
	app.mu.Lock()
	defer app.mu.Unlock()

	for _, ring := range app.results.Rings {
		ring.Scores[category] = InitialRating
		ring.RD[category] = InitialRD
		ring.Volatility[category] = InitialVolatility
		ring.ComparisonCount[category] = 0
	}
	app.results.Comparisons[category] = []Comparison{}

	app.saveResults()
}

// Get ranked rings for a category
func (app *App) getRankedRings(category string) []*Ring {
	app.mu.RLock()
	defer app.mu.RUnlock()

	var rings []*Ring
	for _, ring := range app.results.Rings {
		rings = append(rings, ring)
	}

	sort.Slice(rings, func(i, j int) bool {
		return rings[i].Scores[category] > rings[j].Scores[category]
	})

	return rings
}

// Get category stats
func (app *App) getCategoryStats() map[string]int {
	app.mu.RLock()
	defer app.mu.RUnlock()

	stats := make(map[string]int)
	for _, cat := range app.getCategoryIDs() {
		stats[cat] = len(app.results.Comparisons[cat])
	}
	return stats
}

// HTTP Handlers

func (app *App) handleIndex(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}

	stats := app.getCategoryStats()
	data := struct {
		Categories     []string
		CategoryLabels map[string]string
		Stats          map[string]int
		TotalRings     int
	}{
		Categories:     app.getCategoryIDs(),
		CategoryLabels: app.getCategoryLabelsMap(),
		Stats:          stats,
		TotalRings:     len(app.results.Rings),
	}

	app.templates.ExecuteTemplate(w, "index.html", data)
}

func (app *App) handleVote(w http.ResponseWriter, r *http.Request) {
	category := strings.TrimPrefix(r.URL.Path, "/vote/")
	if category == "" {
		http.Redirect(w, r, "/", http.StatusSeeOther)
		return
	}

	// Validate category
	validCat := false
	for _, cat := range app.getCategoryIDs() {
		if cat == category {
			validCat = true
			break
		}
	}
	if !validCat {
		http.Redirect(w, r, "/", http.StatusSeeOther)
		return
	}

	if r.Method == http.MethodPost {
		// Handle vote submission
		ringA := r.FormValue("ring_a")
		ringB := r.FormValue("ring_b")
		winner := r.FormValue("winner")

		if winner != "" {
			// Handle all vote types: ringA, ringB, same, skip
			app.recordVote(category, ringA, ringB, winner)
		}

		// Return next matchup partial for HTMX
		if r.Header.Get("HX-Request") == "true" {
			app.renderMatchup(w, category)
			return
		}
	}

	// Render full vote page
	ringA, ringB, ok := app.getNextMatchup(category)
	if !ok {
		http.Redirect(w, r, "/results", http.StatusSeeOther)
		return
	}

	stats := app.getCategoryStats()
	confidence := app.getCategoryConfidence(category)
	data := struct {
		Category      string
		CategoryLabel string
		Prompt        string
		RingA         string
		RingB         string
		Progress      int
		TotalRings    int
		Confidence    float64
	}{
		Category:      category,
		CategoryLabel: app.getCategoryLabel(category),
		Prompt:        app.getCategoryPrompt(category),
		RingA:         ringA,
		RingB:         ringB,
		Progress:      stats[category],
		TotalRings:    len(app.results.Rings),
		Confidence:    confidence,
	}

	app.templates.ExecuteTemplate(w, "vote.html", data)
}

func (app *App) renderMatchup(w http.ResponseWriter, category string) {
	ringA, ringB, ok := app.getNextMatchup(category)
	if !ok {
		w.Header().Set("HX-Redirect", "/results")
		return
	}

	stats := app.getCategoryStats()
	confidence := app.getCategoryConfidence(category)
	data := struct {
		Category      string
		CategoryLabel string
		Prompt        string
		RingA         string
		RingB         string
		Progress      int
		TotalRings    int
		Confidence    float64
	}{
		Category:      category,
		CategoryLabel: app.getCategoryLabel(category),
		Prompt:        app.getCategoryPrompt(category),
		RingA:         ringA,
		RingB:         ringB,
		Progress:      stats[category],
		TotalRings:    len(app.results.Rings),
		Confidence:    confidence,
	}

	app.templates.ExecuteTemplate(w, "matchup", data)
}

func (app *App) handleResults(w http.ResponseWriter, r *http.Request) {
	rankings := make(map[string][]*Ring)
	for _, cat := range app.getCategoryIDs() {
		rankings[cat] = app.getRankedRings(cat)
	}

	stats := app.getCategoryStats()

	// Calculate confidence for each category
	confidence := make(map[string]int)
	for _, cat := range app.getCategoryIDs() {
		confidence[cat] = int(app.getCategoryConfidence(cat))
	}

	data := struct {
		Categories     []string
		CategoryLabels map[string]string
		Rankings       map[string][]*Ring
		Stats          map[string]int
		Confidence     map[string]int
	}{
		Categories:     app.getCategoryIDs(),
		CategoryLabels: app.getCategoryLabelsMap(),
		Rankings:       rankings,
		Stats:          stats,
		Confidence:     confidence,
	}

	app.templates.ExecuteTemplate(w, "results.html", data)
}

func (app *App) handleReset(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Redirect(w, r, "/", http.StatusSeeOther)
		return
	}

	category := strings.TrimPrefix(r.URL.Path, "/reset/")
	app.resetCategory(category)

	if r.Header.Get("HX-Request") == "true" {
		w.Header().Set("HX-Redirect", "/vote/"+category)
		return
	}

	http.Redirect(w, r, "/vote/"+category, http.StatusSeeOther)
}

func (app *App) handleExport(w http.ResponseWriter, r *http.Request) {
	app.mu.RLock()
	defer app.mu.RUnlock()

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Content-Disposition", "attachment; filename=ring_preferences.json")

	json.NewEncoder(w).Encode(app.results)
}

// Report data structures
type ReportRing struct {
	Rank      int
	Filename  string
	ImagePath string
	FileLink  string
	Score     float64
}

type ReportCategory struct {
	Name        string
	Label       string
	Comparisons int
	Rings       []ReportRing
}

type RingSummary struct {
	Filename      string
	ImagePath     string
	FileLink      string
	CategoryRanks []string // "5/54" format for each category
	TotalScore    int      // Sum of all scores
	AvgRank       string   // Average rank across categories
}

type ReportData struct {
	GeneratedAt              string
	TotalComparisons         int
	TotalRings               int
	ShowTopOnly              bool
	TopCount                 int
	SingleCategory           bool
	SingleCategoryLabel      string
	Categories               []ReportCategory
	ShowCrossCategorySummary bool
	RingSummaries            []RingSummary
}

func (app *App) handleReport(w http.ResponseWriter, r *http.Request) {
	// Parse query parameters
	scope := r.URL.Query().Get("scope")       // "top10" or "all"
	category := r.URL.Query().Get("category") // specific category or "all"

	if scope == "" {
		scope = "top10"
	}
	if category == "" {
		category = "all"
	}

	showTopOnly := scope == "top10"
	topCount := 10

	// Build report data
	app.mu.RLock()

	reportData := ReportData{
		GeneratedAt:      time.Now().Format("January 2, 2006 at 3:04 PM"),
		TotalRings:       len(app.results.Rings),
		ShowTopOnly:      showTopOnly,
		TopCount:         topCount,
		SingleCategory:   category != "all",
		TotalComparisons: 0,
	}

	// Determine which categories to include
	var categoriesToInclude []string
	if category == "all" {
		categoriesToInclude = app.getCategoryIDs()
	} else {
		// Validate category
		valid := false
		for _, cat := range app.getCategoryIDs() {
			if cat == category {
				valid = true
				break
			}
		}
		if !valid {
			app.mu.RUnlock()
			http.Error(w, "Invalid category", http.StatusBadRequest)
			return
		}
		categoriesToInclude = []string{category}
		reportData.SingleCategoryLabel = app.getCategoryLabel(category)
	}

	// Get working directory for absolute paths
	workDir, err := os.Getwd()
	if err != nil {
		app.mu.RUnlock()
		http.Error(w, "Failed to get working directory", http.StatusInternalServerError)
		return
	}

	// Build category data
	for _, cat := range categoriesToInclude {
		ranked := app.getRankedRingsUnlocked(cat)
		comparisons := len(app.results.Comparisons[cat])
		reportData.TotalComparisons += comparisons

		reportCat := ReportCategory{
			Name:        cat,
			Label:       app.getCategoryLabel(cat),
			Comparisons: comparisons,
			Rings:       []ReportRing{},
		}

		limit := len(ranked)
		if showTopOnly && limit > topCount {
			limit = topCount
		}

		for i := 0; i < limit; i++ {
			ring := ranked[i]
			// Use relative path that will work with symlinked rings folder
			imagePath := "rings/" + ring.Filename
			// Absolute path for clickable file link
			fileLink := filepath.Join(workDir, app.ringsDir, ring.Filename)
			reportCat.Rings = append(reportCat.Rings, ReportRing{
				Rank:      i + 1,
				Filename:  ring.Filename,
				ImagePath: imagePath,
				FileLink:  fileLink,
				Score:     ring.Scores[cat],
			})
		}

		reportData.Categories = append(reportData.Categories, reportCat)
	}

	// Build cross-category summary if showing all categories
	if !reportData.SingleCategory && len(categoriesToInclude) > 1 {
		reportData.ShowCrossCategorySummary = true

		// Build rank map for each ring in each category using FULL rankings (not just top N)
		ringRanks := make(map[string]map[string]int)    // filename -> category -> rank
		ringScores := make(map[string]map[string]int)   // filename -> category -> score

		for _, cat := range categoriesToInclude {
			// Get full rankings for this category
			ranked := app.getRankedRingsUnlocked(cat)
			for i, ring := range ranked {
				if ringRanks[ring.Filename] == nil {
					ringRanks[ring.Filename] = make(map[string]int)
					ringScores[ring.Filename] = make(map[string]int)
				}
				ringRanks[ring.Filename][cat] = i + 1
				ringScores[ring.Filename][cat] = int(ring.Scores[cat])
			}
		}

		// Create summaries for each ring
		type summaryWithAvg struct {
			summary RingSummary
			avgRank float64
		}
		var summaries []summaryWithAvg

		for filename := range app.results.Rings {
			imagePath := "rings/" + filename
			fileLink := filepath.Join(workDir, app.ringsDir, filename)

			var ranks []string
			var totalScore int
			var rankSum int
			rankCount := 0

			for _, cat := range categoriesToInclude {
				if rank, ok := ringRanks[filename][cat]; ok {
					ranks = append(ranks, fmt.Sprintf("%d", rank))
					totalScore += ringScores[filename][cat]
					rankSum += rank
					rankCount++
				} else {
					ranks = append(ranks, "-")
				}
			}

			// Skip rings with no rankings at all
			if rankCount == 0 {
				continue
			}

			avgRank := float64(rankSum) / float64(rankCount)
			avgRankStr := fmt.Sprintf("%.1f", avgRank)

			summaries = append(summaries, summaryWithAvg{
				summary: RingSummary{
					Filename:      filename,
					ImagePath:     imagePath,
					FileLink:      fileLink,
					CategoryRanks: ranks,
					TotalScore:    totalScore,
					AvgRank:       avgRankStr,
				},
				avgRank: avgRank,
			})
		}

		// Sort by average rank (best first)
		sort.Slice(summaries, func(i, j int) bool {
			return summaries[i].avgRank < summaries[j].avgRank
		})

		for _, s := range summaries {
			reportData.RingSummaries = append(reportData.RingSummaries, s.summary)
		}
	}

	app.mu.RUnlock()

	// Generate Typst file from embedded template
	tmpl, err := text_template.ParseFS(embeddedFS, "templates/report/report.typ.tmpl")
	if err != nil {
		log.Printf("Failed to parse report template: %v", err)
		http.Error(w, "Failed to parse report template", http.StatusInternalServerError)
		return
	}

	// Create temp directory for report
	tempDir, err := os.MkdirTemp("", "ring-report-*")
	if err != nil {
		http.Error(w, "Failed to create temp directory", http.StatusInternalServerError)
		return
	}
	defer os.RemoveAll(tempDir)

	// Symlink rings directory into temp dir for Typst to find images
	ringsSymlink := filepath.Join(tempDir, "rings")
	ringsAbsPath := filepath.Join(workDir, app.ringsDir)
	if err := os.Symlink(ringsAbsPath, ringsSymlink); err != nil {
		log.Printf("Failed to create symlink: %v", err)
		http.Error(w, "Failed to setup report directory", http.StatusInternalServerError)
		return
	}

	// Generate .typ file
	typFile := filepath.Join(tempDir, "report.typ")
	var typContent bytes.Buffer
	if err := tmpl.Execute(&typContent, reportData); err != nil {
		log.Printf("Failed to execute template: %v", err)
		http.Error(w, "Failed to generate report", http.StatusInternalServerError)
		return
	}

	if err := os.WriteFile(typFile, typContent.Bytes(), 0644); err != nil {
		http.Error(w, "Failed to write report file", http.StatusInternalServerError)
		return
	}

	// Compile with Typst
	pdfFile := filepath.Join(tempDir, "report.pdf")
	cmd := exec.Command("typst", "compile", typFile, pdfFile)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		log.Printf("Typst compilation failed: %v\nStderr: %s", err, stderr.String())
		log.Printf("Typst source:\n%s", typContent.String())
		http.Error(w, fmt.Sprintf("Failed to compile PDF: %s", stderr.String()), http.StatusInternalServerError)
		return
	}

	// Read and serve PDF
	pdfData, err := os.ReadFile(pdfFile)
	if err != nil {
		http.Error(w, "Failed to read generated PDF", http.StatusInternalServerError)
		return
	}

	// Generate filename
	filename := "ring-preferences"
	if category != "all" {
		filename += "-" + category
	}
	if showTopOnly {
		filename += "-top10"
	} else {
		filename += "-full"
	}
	filename += ".pdf"

	w.Header().Set("Content-Type", "application/pdf")
	w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%s", filename))
	w.Header().Set("Content-Length", fmt.Sprintf("%d", len(pdfData)))
	w.Write(pdfData)
}

// getRankedRingsUnlocked returns ranked rings without acquiring lock (caller must hold lock)
func (app *App) getRankedRingsUnlocked(category string) []*Ring {
	var rings []*Ring
	for _, ring := range app.results.Rings {
		rings = append(rings, ring)
	}

	sort.Slice(rings, func(i, j int) bool {
		return rings[i].Scores[category] > rings[j].Scores[category]
	})

	return rings
}

// Serve the report options page
func (app *App) handleReportPage(w http.ResponseWriter, r *http.Request) {
	stats := app.getCategoryStats()
	data := struct {
		Categories     []string
		CategoryLabels map[string]string
		Stats          map[string]int
	}{
		Categories:     app.getCategoryIDs(),
		CategoryLabels: app.getCategoryLabelsMap(),
		Stats:          stats,
	}

	app.templates.ExecuteTemplate(w, "report.html", data)
}

// Handle undo last vote
func (app *App) handleUndo(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Redirect(w, r, "/", http.StatusSeeOther)
		return
	}

	category := strings.TrimPrefix(r.URL.Path, "/undo/")
	if category == "" {
		http.Error(w, "Category required", http.StatusBadRequest)
		return
	}

	ringA, ringB, success := app.undoLastVote(category)

	if r.Header.Get("HX-Request") == "true" {
		if success {
			// Render the undone matchup so user can revote on it
			app.renderMatchupWithPair(w, category, ringA, ringB)
		} else {
			w.Write([]byte(`<div class="error">No votes to undo</div>`))
		}
		return
	}

	http.Redirect(w, r, "/vote/"+category, http.StatusSeeOther)
}

// Render a specific matchup (used for undo to show the same pair again)
func (app *App) renderMatchupWithPair(w http.ResponseWriter, category, ringA, ringB string) {
	stats := app.getCategoryStats()
	confidence := app.getCategoryConfidence(category)
	data := struct {
		Category      string
		CategoryLabel string
		Prompt        string
		RingA         string
		RingB         string
		Progress      int
		TotalRings    int
		Confidence    float64
	}{
		Category:      category,
		CategoryLabel: app.getCategoryLabel(category),
		Prompt:        app.getCategoryPrompt(category),
		RingA:         ringA,
		RingB:         ringB,
		Progress:      stats[category],
		TotalRings:    len(app.results.Rings),
		Confidence:    confidence,
	}

	app.templates.ExecuteTemplate(w, "matchup", data)
}

// Get average confidence for a category (100 = fully confident, 0 = no confidence)
func (app *App) getCategoryConfidence(category string) float64 {
	app.mu.RLock()
	defer app.mu.RUnlock()

	if len(app.results.Rings) == 0 {
		return 0
	}

	totalConfidence := 0.0
	for _, ring := range app.results.Rings {
		// Convert RD to confidence percentage
		// RD of 350 (max) = 0% confidence, RD of 30 (min) = 100% confidence
		rd := ring.RD[category]
		confidence := 100.0 * (1.0 - (rd-MinRD)/(MaxRD-MinRD))
		totalConfidence += confidence
	}

	return totalConfidence / float64(len(app.results.Rings))
}

// Image Management Handlers
func (app *App) handleManage(w http.ResponseWriter, r *http.Request) {
	// Get list of all images
	files, err := os.ReadDir(app.ringsDir)
	if err != nil {
		http.Error(w, "Failed to read images directory", http.StatusInternalServerError)
		return
	}

	type ImageInfo struct {
		Filename    string
		HasRankings bool
	}

	var images []ImageInfo
	app.mu.RLock()
	for _, f := range files {
		if f.IsDir() {
			continue
		}
		ext := strings.ToLower(filepath.Ext(f.Name()))
		if ext == ".png" || ext == ".jpg" || ext == ".jpeg" || ext == ".gif" || ext == ".webp" {
			hasRankings := false
			if ring, exists := app.results.Rings[f.Name()]; exists {
				// Check if ring has any comparisons
				for _, count := range ring.ComparisonCount {
					if count > 0 {
						hasRankings = true
						break
					}
				}
			}
			images = append(images, ImageInfo{
				Filename:    f.Name(),
				HasRankings: hasRankings,
			})
		}
	}
	app.mu.RUnlock()

	categories := app.getCategories()

	app.templates.ExecuteTemplate(w, "manage.html", map[string]interface{}{
		"Images":        images,
		"ImageCount":    len(images),
		"Categories":    categories,
		"CategoryCount": len(categories),
	})
}

func (app *App) handleUpload(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Parse multipart form (32MB max)
	if err := r.ParseMultipartForm(32 << 20); err != nil {
		http.Error(w, "Failed to parse form", http.StatusBadRequest)
		return
	}

	files := r.MultipartForm.File["images"]
	if len(files) == 0 {
		http.Error(w, "No files uploaded", http.StatusBadRequest)
		return
	}

	uploaded := 0
	for _, fileHeader := range files {
		// Validate extension
		ext := strings.ToLower(filepath.Ext(fileHeader.Filename))
		if ext != ".png" && ext != ".jpg" && ext != ".jpeg" && ext != ".gif" && ext != ".webp" {
			continue
		}

		// Open uploaded file
		file, err := fileHeader.Open()
		if err != nil {
			continue
		}
		defer file.Close()

		// Create destination file
		dstPath := filepath.Join(app.ringsDir, fileHeader.Filename)
		dst, err := os.Create(dstPath)
		if err != nil {
			continue
		}
		defer dst.Close()

		// Copy
		if _, err := io.Copy(dst, file); err != nil {
			continue
		}

		// Initialize ring in results
		app.mu.Lock()
		if app.results.Rings[fileHeader.Filename] == nil {
			app.results.Rings[fileHeader.Filename] = &Ring{
				Filename:        fileHeader.Filename,
				Scores:          make(map[string]float64),
				RD:              make(map[string]float64),
				Volatility:      make(map[string]float64),
				ComparisonCount: make(map[string]int),
			}
			for _, cat := range app.getCategoryIDs() {
				app.results.Rings[fileHeader.Filename].Scores[cat] = InitialRating
				app.results.Rings[fileHeader.Filename].RD[cat] = InitialRD
				app.results.Rings[fileHeader.Filename].Volatility[cat] = InitialVolatility
			}
		}
		app.mu.Unlock()

		uploaded++
	}

	// Save results
	app.saveResults()

	// Return JSON response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"success":  true,
		"uploaded": uploaded,
	})
}

func (app *App) handleDeleteImage(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	filename := r.FormValue("filename")
	if filename == "" {
		http.Error(w, "Filename required", http.StatusBadRequest)
		return
	}

	// Security: prevent path traversal
	if strings.Contains(filename, "/") || strings.Contains(filename, "\\") || strings.Contains(filename, "..") {
		http.Error(w, "Invalid filename", http.StatusBadRequest)
		return
	}

	// Delete file
	filePath := filepath.Join(app.ringsDir, filename)
	if err := os.Remove(filePath); err != nil {
		http.Error(w, "Failed to delete file", http.StatusInternalServerError)
		return
	}

	// Remove from results
	app.mu.Lock()
	delete(app.results.Rings, filename)
	// Remove from comparisons
	for cat := range app.results.Comparisons {
		var filtered []Comparison
		for _, c := range app.results.Comparisons[cat] {
			if c.RingA != filename && c.RingB != filename {
				filtered = append(filtered, c)
			}
		}
		app.results.Comparisons[cat] = filtered
	}
	app.mu.Unlock()

	// Save results
	app.saveResults()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"success": true,
	})
}

// Category Management Handlers
func (app *App) handleAddCategory(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	id := strings.ToLower(strings.TrimSpace(r.FormValue("id")))
	label := strings.TrimSpace(r.FormValue("label"))
	prompt := strings.TrimSpace(r.FormValue("prompt"))

	if id == "" || label == "" {
		http.Error(w, "ID and label required", http.StatusBadRequest)
		return
	}

	// Validate ID (alphanumeric and dashes only)
	for _, c := range id {
		if !((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-') {
			http.Error(w, "ID must be lowercase alphanumeric with dashes only", http.StatusBadRequest)
			return
		}
	}

	if prompt == "" {
		prompt = "Which do you prefer?"
	}

	app.mu.Lock()
	// Check for duplicate
	for _, cat := range app.results.Categories {
		if cat.ID == id {
			app.mu.Unlock()
			http.Error(w, "Category ID already exists", http.StatusBadRequest)
			return
		}
	}

	// Add category
	app.results.Categories = append(app.results.Categories, Category{
		ID:     id,
		Label:  label,
		Prompt: prompt,
	})

	// Initialize comparisons for new category
	if app.results.Comparisons == nil {
		app.results.Comparisons = make(map[string][]Comparison)
	}
	app.results.Comparisons[id] = []Comparison{}

	// Initialize category for all existing rings
	for _, ring := range app.results.Rings {
		ring.Scores[id] = InitialRating
		ring.RD[id] = InitialRD
		ring.Volatility[id] = InitialVolatility
		ring.ComparisonCount[id] = 0
	}
	app.mu.Unlock()

	app.saveResults()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"success": true,
		"id":      id,
	})
}

func (app *App) handleDeleteCategory(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	id := r.FormValue("id")
	if id == "" {
		http.Error(w, "Category ID required", http.StatusBadRequest)
		return
	}

	app.mu.Lock()
	// Find and remove category
	found := false
	newCategories := []Category{}
	for _, cat := range app.results.Categories {
		if cat.ID == id {
			found = true
		} else {
			newCategories = append(newCategories, cat)
		}
	}

	if !found {
		app.mu.Unlock()
		http.Error(w, "Category not found", http.StatusNotFound)
		return
	}

	if len(newCategories) == 0 {
		app.mu.Unlock()
		http.Error(w, "Cannot delete last category", http.StatusBadRequest)
		return
	}

	app.results.Categories = newCategories

	// Remove comparisons for this category
	delete(app.results.Comparisons, id)

	// Remove category data from all rings
	for _, ring := range app.results.Rings {
		delete(ring.Scores, id)
		delete(ring.RD, id)
		delete(ring.Volatility, id)
		delete(ring.ComparisonCount, id)
	}
	app.mu.Unlock()

	app.saveResults()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"success": true,
	})
}

func (app *App) handleUpdateCategory(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	id := r.FormValue("id")
	label := strings.TrimSpace(r.FormValue("label"))
	prompt := strings.TrimSpace(r.FormValue("prompt"))

	if id == "" || label == "" {
		http.Error(w, "ID and label required", http.StatusBadRequest)
		return
	}

	app.mu.Lock()
	found := false
	for i, cat := range app.results.Categories {
		if cat.ID == id {
			app.results.Categories[i].Label = label
			if prompt != "" {
				app.results.Categories[i].Prompt = prompt
			}
			found = true
			break
		}
	}
	app.mu.Unlock()

	if !found {
		http.Error(w, "Category not found", http.StatusNotFound)
		return
	}

	app.saveResults()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"success": true,
	})
}

func main() {
	rand.Seed(time.Now().UnixNano())

	app := NewApp()

	// Routes
	http.HandleFunc("/", app.handleIndex)
	http.HandleFunc("/vote/", app.handleVote)
	http.HandleFunc("/results", app.handleResults)
	http.HandleFunc("/reset/", app.handleReset)
	http.HandleFunc("/undo/", app.handleUndo)
	http.HandleFunc("/export", app.handleExport)
	http.HandleFunc("/report", app.handleReportPage)
	http.HandleFunc("/report/generate", app.handleReport)
	http.HandleFunc("/manage", app.handleManage)
	http.HandleFunc("/upload", app.handleUpload)
	http.HandleFunc("/delete-image", app.handleDeleteImage)
	http.HandleFunc("/add-category", app.handleAddCategory)
	http.HandleFunc("/delete-category", app.handleDeleteCategory)
	http.HandleFunc("/update-category", app.handleUpdateCategory)

	// Static files - rings from disk (user data), static assets from embedded FS
	http.Handle("/rings/", http.StripPrefix("/rings/", http.FileServer(http.Dir("rings"))))
	staticFS, err := fs.Sub(embeddedFS, "static")
	if err != nil {
		log.Fatal("Failed to create static sub-filesystem:", err)
	}
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.FS(staticFS))))

	log.Println("RingRank running at http://localhost:8089")
	log.Fatal(http.ListenAndServe(":8089", nil))
}
