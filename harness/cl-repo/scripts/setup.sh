#!/usr/bin/env bash
# Install the published cl-repo clients this demo compares (default: 0.27.34
# and 0.27.35) into harness/cl-repo/.clients/<version>/, which git ignores.
# Each comes from its OCI image on GHCR through that release's own
# setup-client.sh. Nothing else is touched: not ~/.cl-repository-client, not
# ~/.local/bin/cl-repo. Needs oras, python3 and curl. Re-run to reinstall.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
versions=("$@")
if [[ ${#versions[@]} -eq 0 ]]; then
  versions=(0.27.34 0.27.35)
fi

for version in "${versions[@]}"; do
  dest="${here}/.clients/${version}"
  work="$(mktemp -d)"
  curl -fsSL -o "${work}/setup-client.sh" \
    "https://raw.githubusercontent.com/egao1980/cl-repository/v${version}/.github/actions/setup-client/setup-client.sh"
  echo "cl-repo ${version} -> ${dest}"
  (cd "${work}" && env -u GITHUB_ENV -u GITHUB_OUTPUT \
     GITHUB_WORKSPACE="${work}" CL_REPOSITORY_DEST="${dest}" \
     CL_REPOSITORY_CLIENT_VERSION="${version}" bash setup-client.sh > "${work}/setup.log" 2>&1) \
    || { tail -20 "${work}/setup.log"; rm -rf "${work}"; exit 1; }
  rm -rf "${work}"
  test -f "${dest}/cl-oci-${version}/roswell/cl-repo" \
    || { echo "setup.sh: ${dest}/cl-oci-${version}/roswell/cl-repo is missing" >&2; exit 1; }
done
