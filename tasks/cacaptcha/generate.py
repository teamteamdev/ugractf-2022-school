#!/usr/bin/env python3

import hmac
import json
import os
import random
import sys

PREFIX = "ugra_did_you_know_that_captcha_is_a_trademark_"
FLAG_SECRET = b"surface-blue-divided-deep-forever-made-realize-fire-speed-separate"
SALT_SIZE = 16


def get_flag(user_id):
    return PREFIX + hmac.new(FLAG_SECRET, str(user_id).encode(), "sha256").hexdigest()[:SALT_SIZE]


def generate():
    if len(sys.argv) < 3:
        print("Usage: generate.py user_id target_dir", file=sys.stderr)
        sys.exit(1)

    user_id = sys.argv[1]
    flag = get_flag(user_id)

    json.dump({"flags": [flag]}, sys.stdout)


if __name__ == "__main__":
    generate()
