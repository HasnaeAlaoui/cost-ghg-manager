#!/bin/sh
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd) || exit 1
case "$(/usr/bin/uname -m)" in
  arm64) SERVER=server-macos-arm64 ;;
  x86_64) SERVER=server-macos-amd64 ;;
  *) printf '%s\n' 'This package supports Apple silicon and Intel Macs.'; exit 1 ;;
esac
printf '%s\n' "Starting $SERVER"
"$ROOT/vendor/launcher/$SERVER"
STATUS=$?
if [ "$STATUS" -ne 0 ]; then
  printf '%s\n' "The local server ($SERVER) could not start."
  printf '%s\n' 'If macOS showed an unidentified-developer warning for this file:'
  printf '%s\n' 'For your trusted course copy, open System Settings > Privacy & Security.'
  printf '%s\n' 'Use Open Anyway for the named file, confirm Open, then run START-MAC.command again.'
  printf '%s\n' 'You do not need to navigate into vendor/. If approval is unavailable, contact your instructor.'
  printf '%s\n' 'For other errors, see the message above or contact your instructor.'
  printf '%s' 'Press Return to close this window.'
  read -r REPLY
fi
exit "$STATUS"
