#!/usr/bin/env bash
# Run harness-demos-s3-minio against a throwaway MinIO container.
#
# Makes up an access key and a secret that holds # and ; (never printed),
# starts MinIO with them on a random loopback port, creates the bucket, runs
# the demo with the keys in AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY, and
# removes the container on exit. Needs docker (on macOS e.g. OrbStack:
# `docker context use orbstack`), cl-repo and openssl; uv is optional
# (section 2's cross-read with Python's configparser).
#
#   harness/s3-minio/scripts/run.sh > harness/s3-minio/demo-output.txt 2>&1
set -euo pipefail

here=$(cd "$(dirname "$0")/.." && pwd)
# Chainguard's MinIO build (the minio/minio image is no longer published).
image=${S3_DEMO_IMAGE:-cgr.dev/chainguard/minio@sha256:bd92a45b821c0d85772c607ed7b49693efe78b3ffffae301b5d1b0981660de22}
name=cl-stack-demo-minio-$$
bucket=cl-stack-demo
access="demo$(openssl rand -hex 6)"
secret="demo#$(openssl rand -hex 8);$(openssl rand -hex 8)"

cleanup() { docker rm -f "$name" >/dev/null 2>&1 || true; }
trap cleanup EXIT

docker run -d --name "$name" -p 127.0.0.1::9000 \
  -e MINIO_ROOT_USER="$access" -e MINIO_ROOT_PASSWORD="$secret" \
  "$image" server /tmp/minio-data >/dev/null
port=$(docker port "$name" 9000/tcp | head -n 1 | sed 's/.*://')
endpoint="http://127.0.0.1:$port"
curl -fs --retry 60 --retry-delay 1 --retry-all-errors "$endpoint/minio/health/ready" >/dev/null \
  || { echo "MinIO at $endpoint did not become ready" >&2; exit 1; }
docker exec "$name" mc alias set demo http://127.0.0.1:9000 "$access" "$secret" >/dev/null
docker exec "$name" mc mb "demo/$bucket" >/dev/null
minio_version=$(docker exec "$name" minio --version | head -n 1)

cd "$here"
S3_DEMO_ENDPOINT="$endpoint" S3_DEMO_BUCKET="$bucket" S3_DEMO_MINIO="$minio_version" \
AWS_ACCESS_KEY_ID="$access" AWS_SECRET_ACCESS_KEY="$secret" \
  cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
    --eval '(asdf:load-system "harness-demos-s3-minio")' \
    --eval '(unless (harness-demos-s3-minio:run-demos) (uiop:quit 1))'
