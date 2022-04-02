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

import cacaptcha as caca
from levelnames import level_name

BASE_DIR = os.path.dirname(__file__)
PREFIX = "ugra_did_you_know_that_captcha_is_a_trademark_"
FLAG_SECRET = b"surface-blue-divided-deep-forever-made-realize-fire-speed-separate"
SALT_SIZE = 16

with open('corpus.txt') as f:
    CORPUS = f.read()
MAX_LEVEL = 1337

@web.middleware
async def custom_headers(request, handler):
    response = await handler(request)
    response.headers.setdefault('X-Protected-By', 'HAXORWALL v. 3.43 by l4in3r')
    return response


def format_exception(code, ex, comment):
    return f'(HTTP-EXCEPTION\n  :STATUS-CODE {code}\n  :ERROR \'{ex}\n  :MESSAGE "{comment}")'


def get_flag(user_id):
    return PREFIX + hmac.new(FLAG_SECRET, str(user_id).encode(), "sha256").hexdigest()[:SALT_SIZE]


async def parse_session(request):
    token = request.match_info["token"]
    session = await get_session(request)
    if session.get('token') != token or not session.get('level'):
        session['token'] = token
        session['level'] = 1
        session['lamerness'] = 0
    if session.get('lamerness') > 75:
        session.invalidate()
        raise web.HTTPForbidden(text=format_exception(
            403, 'USER-TOO-LAME-ERROR',
            "Ты по ходу L4M3R... Запрогай свою тачку ПРАВИЛЬНО!!!"
        ))
    session['level_name'] = level_name(session['level'])
    return session


async def generate_regexes(state):
    lvl = state['level']
    seed = f"{state.get('token')}{lvl}saltysaltomg"
    count = round(lvl / (3 * math.log(MAX_LEVEL))) + 2
    return caca.generate(CORPUS, seed, n=count)


def make_app():
    app = web.Application(middlewares=[custom_headers])
    routes = web.RouteTableDef()
    setup(
        app,
        EncryptedCookieStorage(FLAG_SECRET[10:42], cookie_name="haxorwall_session")
    )

    @routes.get("/{token}/")
    async def main(request):
        a_time = round(time() * 1000000)
        return jinja2.render_template("index.html", request, {
            'time': round(time() * 1000000) - a_time
        })


    @routes.get("/{token}/game")
    async def game(request):
        a_time = round(time() * 1000000)
        state = await parse_session(request)
        rules = "(ALL\n{})".format(
            '\n'.join([f'  (MUST-CONTAIN :REGEX /{r.regex}/)' for r in await generate_regexes(state)])
        )


        return jinja2.render_template("game.html", request, {
            'time': round(time() * 1000000) - a_time,
            'request': request,
            'state': state,
            'debug': os.environ.get("DEBUG") == "F",
            'rules': rules,
            'flag': get_flag(state['token']) if state['level'] == MAX_LEVEL else None
        })


    @routes.post("/{token}/game")
    async def test_game(request):
        state = await parse_session(request)
        form = await request.post()
        content = form.get("data", "")
        regexes = await generate_regexes(state)
        matches = [print(re.search(r.regex, content), r.regex) or re.search(r.regex, content) for r in regexes]
        if all(matches):
            state['level'] += 1
        else:
            state['lamerness'] += 25
            # state['level'] = MAX_LEVEL - 1
        raise web.HTTPFound(f"/{state['token']}/game")


    app.add_routes(routes)
    app.router.add_static('/static/', path='./static/', name='static')
    jinja2.setup(app, loader=FileSystemLoader(os.path.join(BASE_DIR, "templates")))
    return app


if __name__ == "__main__":
    app = make_app()

    if os.environ.get("DEBUG") == "F":
        web.run_app(app, host="0.0.0.0", port=31337)
    else:
        web.run_app(app, path=os.path.join(STATE_DIR, "cacaptcha.sock"))
