import exrex
import re
import requests
import html

TOKEN = '99c7dd88de68198f'
URL = f'https://cacaptcha.s.2022.ugractf.ru/{TOKEN}/game'


def parse_page(page: str) -> dict:
    level = re.search(r'Уровень: ([0-9]+)/', page)
    lamerness = re.search(r'L4MER L3VEL: ([0-9]+)%', page)
    flag = re.search(r'(ugra_[A-Za-z0-9_]+)', page)
    if level and lamerness:
        level = int(level.groups()[0])
        lamerness = int(lamerness.groups()[0])

    regexes_raw = re.findall(r':REGEX /(.*)/\)', page)
    regexes = []
    for regex in regexes_raw:
        regexes.append(html.unescape(regex))

    return {
        'level': level,
        'lamerness': lamerness,
        'regexes': regexes,
        'flag': flag.groups()[0] if flag else None
    }


def sort_regexes(x: str) -> int:
    if '^' in x:
        return -1
    elif '$' in x:
        return 1
    else:
        return 0


def find_solution(regexes: list[str], **_) -> str:
    regexes = sorted(regexes, key=sort_regexes)
    the_regex = ''.join(regexes)
    return exrex.getone(the_regex)


if __name__ == "__main__":
    session = requests.Session()
    state = parse_page(session.get(URL).text)
    while True:
        print(state['level'], end=' ', flush=True)
        solution = find_solution(**state)
        session.post(URL, data={'data': solution})
        state = parse_page(session.get(URL).text)
        if state['lamerness'] > 0 or state['flag']:
            print(state)
            break
