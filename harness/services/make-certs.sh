#!/usr/bin/env bash
# Test CA + a certificate for 127.0.0.1 for the TLS servers of the demo,
# plus the Mailpit auth file and the TLS-only rabbitmq.conf. Output: certs/
# (git-ignored). Use DEMO_CA=certs/ca.pem when running the demo.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p certs
cd certs
openssl req -x509 -newkey rsa:2048 -nodes -keyout ca.key -out ca.pem -days 30 \
  -subj "/CN=harness-demos-services test CA" 2>/dev/null
openssl req -newkey rsa:2048 -nodes -keyout key.pem -out server.csr -subj "/CN=127.0.0.1" 2>/dev/null
printf 'subjectAltName=IP:127.0.0.1,DNS:localhost\nbasicConstraints=CA:FALSE\nkeyUsage=digitalSignature,keyEncipherment\nextendedKeyUsage=serverAuth\n' > ext.cnf
openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out cert.pem \
  -days 30 -extfile ext.cnf 2>/dev/null
echo "evidence:s3cret" > auth.txt
cat > rabbitmq.conf <<'CONF'
listeners.tcp = none
listeners.ssl.default = 5671
ssl_options.cacertfile = /certs/ca.pem
ssl_options.certfile = /certs/cert.pem
ssl_options.keyfile = /certs/key.pem
ssl_options.verify = verify_none
ssl_options.fail_if_no_peer_cert = false
loopback_users = none
CONF
chmod 644 ./*
ls
