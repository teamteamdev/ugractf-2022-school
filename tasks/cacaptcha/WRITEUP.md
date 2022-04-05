# CACAPTCHA: Write-up

Дают сайт. На сайте просят поиграть в игру, которая состоит из 1337 уровней. В каждом уровне игры даны правила, которым запрос должен удовлетворять:

<table>
<tbody><tr>
<th colspan="2">Параметры HYPERTEXT на всю игру</th>
</tr>
<tr>
<th>Method</th>
<td>POST</td>
</tr>
<tr>
<th>Host</th>
<td>В адресной строке не видно разве?!</td>
</tr>
<tr>
<th>Content-Type</th>
<td>application/x-www-form-urlencoded</td>
</tr>
<tr>
<th colspan="2">Поля в FORM DATA на всю игру</th>
</tr>
<tr>
<th>название поля</th>
<th>содержимое</th>
</tr>
<tr>
<td> data </td>
<td>&lt;произвольное содержимое&gt;</td>
</tr>
<tr>
<th colspan="2">Дополнительные ограничения на всю игру</th>
</tr>
<tr>
<th>Запрос не должен пресекаться фильтром HAX()RW4LL</th>
<th bgcolor="green">+</th>
</tr>
<tr>
<th colspan="2">Конфигурация фильтра HAX()RW4LL для текущего уровня</th>
</tr>
<tr>
<td colspan="2"><pre>(ALL
(MUST-CONTAIN :REGEX /\(II\).*/)
(MUST-CONTAIN :REGEX /.*"Yes, and I wanted to ask you----".*/))</pre></td>
</tr>
</tbody>
<table>

Выходит, нам нужно отправить POST-запрос, содержащий в поле `data` некую строку. Однако, строка не может быть совсем какой угодно: она должна удовлетворять правилам фильтра *HAX()RW4LL*. Разобравшись с синтаксисом, которым описываются эти правила, приходим к выводу: в поле `data` должен быть такой текст, какой будет содержать в себе подстроки, удовлетворяющие данным регулярным выражениям.

Из правил самой игры также следует, что мы можем ошибиться три раза --- тогда уровень «ламерства» перевалит за отметку 75%, и придётся начинать всё сначала.

Задача нехитрая, давайте напишем решалку. План такой:

1. Парсить страницу с игрой, извлекая из неё нужные нам данные: номер текущего уровня, наше «ламерство», список регулярных выражений, а также флаг, если таковой есть.
2. Генерировать строку для поля `data` по данным регулярным выражением.
3. Отправлять POST-запрос.
4. Возвращаться к шагу (1), пока мы не получим флаг.

Чтобы генерировать строку по данному регулярному выражению, можно, конечно, написать свою функцию, которая бы парсила эти выражения на правила и подбирала бы подходящие символы. Но у нас на всё про всё пять часов, а ещё ведь надо и роутер починить, и в крестики-нолики выиграть, и в Питер скататься... Поэтому воспользуемся готовым пакетом [exrex](https://github.com/asciimoo/exrex), который как раз умеет то, что нам надо:

```
>>> exrex.getone('\d{4}-\d{4}-\d{4}-[0-9]{4}')
'3096-7886-2834-5671'
```

Парсить страницу попробуем тоже регулярными выражениями, раз такая тема! Запросы будем отправлять стандартной библиотекой `requests`.

> Стоит, однако, помнить, что регулярными выражениями можно обрабатывать не всё: HTML не является регулярным языком, поскольку, как минимум, содержит открывающие и закрывающие теги, которые создают уровни вложенности. <i>См. подробное объяснение проблемы: <a href="https://stackoverflow.com/a/1732454">You can’t parse HTML with regex</a>.</i>

Приступим:

```python
import exrex    # генератор строк под регулярные выражения
import re       # библиотека для работы с р. в.
import requests
import html     # пригодится позже

TOKEN = '99c7dd88de68198f'
URL = f'https://cacaptcha.s.2022.ugractf.ru/{TOKEN}/game'
```

Напишем функцию, которая будет принимать текст страницы и находить в нём интересующие нас вещи:
```python3
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
```

Затем напишем функцию, которая найдёт подходящее решение. Чтобы удовлетворить правилу `(ALL (MUST-CONTAIN ...) ...)`, достаточно взять все данные выражения, склеить в одно большое простой конкатенацией и скормить функции `exrex.getone`. Но есть один нюанс. В регулярных выражениях есть правила, указывающие, с чего должна начинаться и чем должна заканчиваться строка --- выражениям `^foo` и `foo$` соответственно подойут только строки вида `foo...` и `...foo`. Учтём этот нюанс, отсортировав список данных выражений по вот такому вот [правилу](https://docs.python.org/3/howto/sorting.html#key-functions):

```python
def sort_regexes(x: str) -> int:
    if '^' in x:
        return -1 # должны быть в начале списка
    elif '$' in x:
        return 1  # должны быть в конце
    else:
        return 0  # иначе всё равно
```

Теперь сам поиск решения --- всего три строчки:
```python
def find_solution(regexes: list[str], **_) -> str:
    regexes = sorted(regexes, key=sort_regexes)
    the_regex = ''.join(regexes)
    return exrex.getone(the_regex)
```

Осталось написать цикл, в котором будем засылать решения и получать новые данные:
```python
if __name__ == "__main__":
    session = requests.Session()
    state = parse_page(session.get(URL).text)
    while True:
        print(state['level'], end=' ', flush=True)  # показываем прогресс
        solution = find_solution(**state)
        session.post(URL, data={'data': solution})  # засылаем ответ
        state = parse_page(session.get(URL).text)
        if state['lamerness'] > 0 or state['flag']: # в нештатной ситуации останавливаемся и смотрим, что там пришло
            print(state)
            break
```

Запустим скрипт и посмотрим, какой же был флаг:
```
cacaptcha § python solver.py
1 2 3 4 5 6 7 8 9 10 11 12 13 [...] 1334 1335 1336 {'level': 1337, 'lamerness': 0, 'regexes': [], 'flag': 'ugra_did_you_know_that_captcha_is_a_trademark_32718c98452a0094'}
```

Ура! В общем-то, не смотря на пугающие тексты на крутой странице крутого хакера о конечных автоматах и грамматиках, получилось всё не так уж и сложно -- и совсем не страшно.

[Скрипт solver.py](https://github.com/teamteamdev/ugractf-2022-school/blob/master/tasks/cacaptcha/solver.py).

Флаг: **ugra_did_you_know_that_captcha_is_a_trademark_32718c98452a0094**
