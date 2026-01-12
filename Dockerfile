# Build stage
FROM golang:1.21-alpine AS builder

WORKDIR /app

# Copy go mod file if it exists, otherwise init
COPY go.mod* ./
RUN if [ ! -f go.mod ]; then go mod init ringrank; fi

# Copy source code
COPY main.go .

# Build the binary
RUN CGO_ENABLED=0 GOOS=linux go build -o ringrank main.go

# Runtime stage
FROM alpine:3.19

WORKDIR /app

# Install dependencies and Typst for PDF generation
RUN apk add --no-cache ca-certificates curl && \
    ARCH=$(uname -m) && \
    if [ "$ARCH" = "x86_64" ]; then TYPST_ARCH="x86_64"; \
    elif [ "$ARCH" = "aarch64" ]; then TYPST_ARCH="aarch64"; fi && \
    curl -fsSL "https://github.com/typst/typst/releases/download/v0.12.0/typst-${TYPST_ARCH}-unknown-linux-musl.tar.xz" | \
    tar -xJ --strip-components=1 -C /usr/local/bin && \
    apk del curl

# Copy binary from builder
COPY --from=builder /app/ringrank .

# Copy templates and static files
COPY templates/ ./templates/
COPY static/ ./static/

# Create directories for data and images
RUN mkdir -p /app/rings /app/data

# Expose port
EXPOSE 8089

# Volume mounts for persistent data
VOLUME ["/app/rings", "/app/data"]

# Run the application
CMD ["./ringrank"]
