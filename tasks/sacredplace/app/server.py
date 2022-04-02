1#!/usr/bin/env python3

import math
import re
import aiohttp.web as web
from aiohttp_session import get_session, setup
from aiohttp_session.cookie_storage import EncryptedCookieStorage
import aiohttp_jinja2 as jinja2
from jinja2 import FileSystemLoader
from time import time
import hmac
import random
import base64
import os

BASE_DIR = os.path.dirname(__file__)
PREFIX = "ugra_unless_it_is_not_really_that_sacred_"
TOKEN_SECRET = b"abobaabobaabobadaitessss"
TOKEN_SIZE = 16
FLAG_SECRET = b"monte-carlo-computer-technology-mattress-wednesday-journal"
SALT_SIZE = 16


def get_flag(user_id):
    return PREFIX + hmac.new(FLAG_SECRET, str(user_id).encode(), "sha256").hexdigest()[:SALT_SIZE]


def make_app():
    app = web.Application()
    routes = web.RouteTableDef()
    setup(
        app,
        EncryptedCookieStorage(FLAG_SECRET[10:42], cookie_name="dlink_sessid")
    )


    @routes.get("/{token}/")
    async def main(request):
        token = request.match_info["token"]
        session = await get_session(request)
        if session.get('auth'):
            raise web.HTTPFound(f"/{token}/admin")
        return jinja2.render_template("index.html", request, {
        })


    @routes.get("/{token}/admin")
    async def admin(request):
        token = request.match_info["token"]
        session = await get_session(request)
        r = random.Random(token)
        hostnames = ['bra', 'bender', 'Olga-PC(2)', 'iPhone (?????)', 'iPhone (???????)', get_flag(token)]
        r.shuffle(hostnames)
        devices = [{
            'hostname': h,
            'ip': f'192.168.1.{r.randint(4, 240)}',
            'status': 'Active'
        } for h in hostnames]
        if not session.get('auth'):
            raise web.HTTPUnauthorized(text="Consult the DIR-825ACG1 User Manual for further assistance with your Internet Router experience.")
        return jinja2.render_template("admin.html", request, {
            'request': request,
            'devices': devices,
            'flag': get_flag(token)
        })


    @routes.post("/{token}/login")
    async def login(request):
        token = request.match_info["token"]
        session = await get_session(request)
        form = await request.post()
        login, password = form.get("login", ""), form.get("password", "")
        if login == "admin" and password == "":
            session["auth"] = True
            raise web.HTTPFound(f"/{token}/admin")
        else:
            raise web.HTTPUnauthorized(text="Invalid credentials.")


    app.add_routes(routes)
    app.router.add_static('/static/', path='./static/', name='static')
    jinja2.setup(app, loader=FileSystemLoader(os.path.join(BASE_DIR, "templates")))
    return app


if __name__ == "__main__":
    app = make_app()

    if os.environ.get("DEBUG") == "F":
        web.run_app(app, host="0.0.0.0", port=31337)
    else:
        web.run_app(app, path=os.path.join(STATE_DIR, "sacredplace.sock"))
