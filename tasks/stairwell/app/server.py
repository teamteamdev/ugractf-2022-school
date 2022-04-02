#!/usr/bin/env python3

import aiohttp.web as web
import aiohttp_jinja2 as jinja2

import errno
import hashlib
import hmac
import os
import sys

from jinja2 import FileSystemLoader

BASE_DIR = os.path.dirname(__file__)
STATE_DIR = "/state"
STATIC_DIR = os.path.join(BASE_DIR, "static")

PREFIX = "ugra_and_she_is_buying_one_to_heaven_"
SECRET2 = b"hjdsfaisofpipaw0"
SALT2_SIZE = 12
SECRET3 = b"NVnOvnVNVNVnVnV2"

LAT, LON = 59.935972, 30.356478 - 30.326053  # Pulkovo meridian


def verify_code(code):
    h, sig = code[:40], code[40:]
    signature = hmac.new(SECRET3, h.encode(), "sha256").hexdigest()
    return all(i in "0123456789abcdef" for i in code) and sig == signature


def get_flag(token):
    return PREFIX + hmac.new(SECRET2, token.encode(), "sha256").hexdigest()[:SALT2_SIZE]


def make_app(state_dir):
    app = web.Application()
    routes = web.RouteTableDef()


    @routes.get("/{token}/")
    async def main(request):
        return jinja2.render_template("main.html", request, {})


    @routes.post("/{token}/check")
    async def check(request):
        token = request.match_info["token"]
        data = await request.post()
        lat_s, lon_s, code = data.get("lat", ""), data.get("lon"), data.get("code", "")
        flag = None

        try:
            lat = float(lat_s)
            lon = float(lon_s)
        except:
            lat, lon = None, None
            
        if lat is None:
            status = "answer-invalid"
        elif verify_code(code):
            try:
                fd = os.open(os.path.join(STATE_DIR, code), os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
                with os.fdopen(fd, "w") as f:
                    f.write(repr((token, lat, lon, request.remote, request.headers)))

                if abs(lat - LAT) <= 0.0015/5 and abs(lon - LON) <= 0.00275/5:
                    status = "flag"
                    flag = get_flag(token)
                else:
                    status = "answer-wrong"
            except OSError as e:
                if e.errno == errno.EEXIST:
                    status = "code-used"
                else:
                    raise
        else:
            status = "code-wrong"

        return jinja2.render_template("main.html", request, {"status": status, "flag": flag,
                                                             "lat_s": lat_s, "lon_s": lon_s})


    routes.static("/static", STATIC_DIR)


    app.add_routes(routes)
    jinja2.setup(app, loader=FileSystemLoader(os.path.join(BASE_DIR, "templates")))
    return app


if __name__ == "__main__":
    app = make_app()

    if os.environ.get("DEBUG") == "F":
        web.run_app(app, host="0.0.0.0", port=31337)
    else:
        web.run_app(app, path=os.path.join(STATE_DIR, "stairwell.sock"))
