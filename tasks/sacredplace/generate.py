#!/usr/bin/env python3

import hmac
import json
import os
import random
import sys

PREFIX = "ugra_unless_it_is_not_really_that_sacred_"
TOKEN_SECRET = b"abobaabobaabobadaitessss"
TOKEN_SIZE = 16
FLAG_SECRET = b"monte-carlo-computer-technology-mattress-wednesday-journal"
SALT_SIZE = 16


def get_flag(user_id):
    token = hmac.new(TOKEN_SECRET, str(user_id).encode(), "sha256").hexdigest()[:TOKEN_SIZE]
    flag = PREFIX + hmac.new(FLAG_SECRET, token.encode(), "sha256").hexdigest()[:SALT_SIZE]
    return token, flag


def generate():
    if len(sys.argv) < 3:
        print("Usage: generate.py user_id target_dir", file=sys.stderr)
        sys.exit(1)

    user_id = sys.argv[1]
    token, flag = get_flag(user_id)

    json.dump({"flags": [flag], "urls": [f"https://sacredplace.{{hostname}}/{token}/"]}, sys.stdout)


if __name__ == "__main__":
    generate()
