#!/usr/bin/env bash
# Authenticate to the Human Mortality Database and save a session cookie.
#
# Run this yourself (e.g. in Claude Code, type: ! bash scripts/hmd_login.sh).
# It prompts for your mortality.org email and password; the password is read
# silently and is NOT stored or echoed. On success it writes the session
# cookie to $HMD_COOKIE_FILE (default /tmp/hmd_cookies4.txt), which
# scripts/refresh_hmd_e0.R then consumes.
#
# Adapted from ../hmd-population-discrepancies/README.md.
set -euo pipefail

COOKIE_FILE="${HMD_COOKIE_FILE:-/tmp/hmd_cookies4.txt}"
TMP_PRE="$(mktemp)"
LOGIN_HTML="$(mktemp)"

read -r -p "HMD email: " HMD_EMAIL
read -r -s -p "HMD password: " HMD_PASS; echo

# 1. Fetch login page + pre-auth cookies, extract the anti-forgery token.
curl -s -c "$TMP_PRE" "https://www.mortality.org/Account/Login" > "$LOGIN_HTML"
TOKEN=$(grep -o '__RequestVerificationToken" type="hidden" value="[^"]*"' "$LOGIN_HTML" \
  | sed 's/.*value="//; s/"$//')

if [ -z "$TOKEN" ]; then
  echo "ERROR: could not extract anti-forgery token; HMD login page may have changed." >&2
  rm -f "$TMP_PRE" "$LOGIN_HTML"; exit 1
fi

# 2. POST credentials; capture the authenticated session cookie.
curl -s -c "$COOKIE_FILE" -b "$TMP_PRE" \
  -X POST "https://www.mortality.org/Account/Login" \
  --data-urlencode "Email=$HMD_EMAIL" \
  --data-urlencode "Password=$HMD_PASS" \
  --data-urlencode "__RequestVerificationToken=$TOKEN" \
  --data-urlencode "RememberMe=false" \
  -o /dev/null

rm -f "$TMP_PRE" "$LOGIN_HTML"
unset HMD_PASS

# 3. Verify the cookie works with a test pull.
TEST="$(mktemp)"
curl -s -L -b "$COOKIE_FILE" \
  -o "$TEST" \
  "https://www.mortality.org/File/GetDocument/hmd.v6/GBR_NP/STATS/E0per.txt"
if head -1 "$TEST" | grep -qi "life expectancy\|GBR"; then
  echo "OK: authenticated. Cookie saved to $COOKIE_FILE"
  echo "Now run:  HMD_COOKIE_FILE=$COOKIE_FILE Rscript scripts/refresh_hmd_e0.R"
else
  echo "WARNING: login may have failed (test pull did not return E0per data)." >&2
  echo "First line of response was:" >&2; head -1 "$TEST" >&2
fi
rm -f "$TEST"
