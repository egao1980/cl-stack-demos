# S3 demo: object-store-backend-s3 against a live MinIO

`harness-demos-s3-minio` is a tour of **object-store-backend-s3 0.2.1** from GHCR, run against a real MinIO server in Docker. **cl-stack-config 0.3.1**'s `parse-ini` reads the credentials file, and **http-backend-dexador 0.2.5** sends the requests (cl-stack-http's `:auto` for blocking calls). It shows the botocore credential chain of [object-store-backend-s3#4](https://github.com/egao1980/object-store-backend-s3/issues/4) end to end: first keys from the environment, then keys from an `~/.aws`-style credentials file whose secret holds `#` and `;` and whose key names are upper-case. That is the case from [cl-stack-config#10](https://github.com/egao1980/cl-stack-config/issues/10): parse-ini 0.3.0 signed with the secret cut at `#`, and MinIO answers that request `SignatureDoesNotMatch`. The tour also covers put / get / head / list, a presigned GET ([#7](https://github.com/egao1980/object-store-backend-s3/issues/7)), paged listings, a multipart upload, and delete.

Every result with a known answer is checked, and the summary line reports the mismatches. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run: 25 checks, 0 mismatched, on macOS arm64 with SBCL 2.6.9, OrbStack and MinIO RELEASE.2026-11-23T00-10-37Z. Home and temp paths are replaced by `~` and `<tmp>`.

## Run

```bash
docker context use orbstack        # or any Docker engine
harness/s3-minio/scripts/run.sh > harness/s3-minio/demo-output.txt 2>&1
```

[`scripts/run.sh`](scripts/run.sh) does the following:

1. It makes up an access key and a secret for this run (`demo#<16 hex>;<16 hex>`), with `openssl rand`.
2. It starts `cgr.dev/chainguard/minio`, pinned by digest, with those keys as its root user, on a random `127.0.0.1` port. The `minio/minio` image is no longer published.
3. It creates the bucket `cl-stack-demo` with the `mc` inside the image.
4. It runs the demo through `cl-repo run` with the keys in `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY`.
5. It removes the container on exit, whether the run passes or fails.

The keys are never printed. The output shows only where `#` and `;` are in the secret and the SHA-256 prefixes of the values. The demo points `AWS_SHARED_CREDENTIALS_FILE` and `AWS_CONFIG_FILE` at its own temporary directory, so your `~/.aws` is never read. Section 2's cross-read needs [uv](https://docs.astral.sh/uv/) on `PATH` (`uv run --isolated --no-project python -I`, standard library only). Without uv, that step prints a note and is skipped. `run-demos` returns `NIL` when a check fails or a section stops on an error, and `run.sh` then exits 1.

## What each section shows

1. **Credentials from the environment.** `make-s3-backend` gets no keys, so the first provider that answers is `environment-credentials`. Then `put-object`, `get-object`, `head-object` (size, content type, Last-Modified), `list-objects :prefix`, and a `presign-url` fetched with no credentials through http-protocol, which returns HTTP 200 and the body.
2. **Credentials from a file with `#` and `;` in the secret.** The file has a decoy `[Default]` section and a `[default]` section with `AWS_ACCESS_KEY_ID` (upper case) and `aws_secret_access_key = demo#…;…`. `parse-ini` keeps both section names and lowercases the option names. It reads the whole secret. Python's `configparser.RawConfigParser()`, botocore's reader, reads the same file value for value (same lengths and SHA-256). A request signed with the 4 characters parse-ini 0.3.0 kept comes back as `object-store-error` HTTP 403 `SignatureDoesNotMatch`. Then the environment keys are cleared and `AWS_SHARED_CREDENTIALS_FILE` is set to the file. The chain picks `shared-credentials-file-credentials`, and a put and a get go through with those keys.
3. **Multipart upload.** A 5 MiB part (the S3 minimum for every part but the last) plus a 1 MiB + 123 octet tail are uploaded and completed. The etag ends in `-2`, `head-object` gives the size, and `get-object` returns the two parts in order. Two 1 KiB parts are refused by MinIO at completion (`EntityTooSmall`, as a condition). Then `abort-multipart-upload` runs, and a part sent after the abort finds no upload. The listing shows only the completed object.
4. **Paged listings.** Five objects are listed `:max-keys 2` at a time by following `object-listing-continuation-token`, which gives pages of 2, 2 and 1.
5. **Delete.** `delete-object`, then `get-object` signals `object-not-found` with the key.

## Issues found

The first run, against object-store-backend-s3 0.2.0 and http-backend-dexador 0.2.1, failed two checks. Both bugs are fixed in the versions above:

- [object-store-backend-s3#10](https://github.com/egao1980/object-store-backend-s3/issues/10): `head-object` read neither `Content-Length` nor `Last-Modified`, so a 26-octet object had size 0. Fixed in 0.2.1 ([PR #11](https://github.com/egao1980/object-store-backend-s3/pull/11)).
- [http-backend-dexador#36](https://github.com/egao1980/http-backend-dexador/issues/36): dexador's usocket backend dropped the `Content-Type` of a request with no body. The S3 backend signs that header for `CreateMultipartUpload` (`POST ?uploads`), so MinIO refused every `create-multipart-upload :content-type …` with `AccessDenied: There were headers present in the request which were not signed`. Fixed in 0.2.5 ([PR #37](https://github.com/egao1980/http-backend-dexador/pull/37)).
- [cl-repository#196](https://github.com/egao1980/cl-repository/issues/196): after the `.asd` raised the http-backend-dexador floor to 0.2.5, `cl-repo run` re-resolved the stale `cl-repo.lock`. It kept `http-protocol 0.4.4` although 0.2.5 needs 0.4.5, and ASDF refused to load. Deleting the generated lock worked around it. Still open.
