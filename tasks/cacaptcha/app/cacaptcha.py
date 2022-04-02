import dataclasses
import hashlib
from random import Random
import re
from pprint import pprint as p
from itertools import repeat, chain
from dataclasses import dataclass

TEST: str = "foo 2020-03-12T13:34:56.123Z 'INFO' \\ \"kek\" (привеееет) [org.example.Class]: This is\na #simple #logline containing a 'value'."

GROUPS: dict = {
    "generic_word": r"([^\s]+)",
    "latin_word":   r"[a-zA-Z]+",
    "russian_word": r"[а-яА-Я]+",
    "greek_word":   r"[α-ωΑ-Ω]+",
    "latin_char":   r"[a-zA-Z]",
    "russian_char": r"[а-яА-Я]",
    "greek_char":   r"[α-ωΑ-Ω]",
    "parenthesis":  r"(\([^)]+\))",
    "brackets":     r"(\[[^]]+\])",
    "quote":        r'("[^"]+")',
    "number":       r"[0-9]+",
  # "date":         r"[0-9]{4}-[0-1][0-9]-[0-3][0-9]",
    "repeats":      r"(([^\s])\2+)",
    "first_char":   r"^[^\s]",
    "last_letter":  r"[^\s]$",
    "first_word":   r"^[^\s]+",
    "last_word":    r"[^\s]+$",
    "first_space":  r"^",
    "space":        r"\s{3,}"
}

ESCAPES: dict = {
    "\n": "\\n",
    "\t": "\\t",
    "*": "\\*",
    ".": "\\.",
    ".": "\\.",
    "[": "\\[",
    "]": "\\]",
    "(": "\\(",
    ")": "\\)",
    "{": "\\{",
    "}": "\\}",
    "\\": "\\\\",
    "^": "\^",
    "$": "\$"
}

RANGE_ESCAPES: dict = {
    "[": "\\[",
    "]": "\\]",
    "(": "\\(",
    ")": "\\)",
    "-": "\\-"
}


def escape(s: str, d: dict) -> str:
    result = "".join([d.get(char, char) for char in s])
    return result


def get_alphabet_range(s: str) -> tuple[str, str]:
    chars: list[int] = [ord(char) for char in s]
    a: int = min(chars)
    b: int = max(chars)
    return chr(a), chr(b)


def split_randomly(s: str, n: int, r: Random) -> list[str]:
    split_points = sorted(r.sample(range(len(s)), n-1))
    split_ranges = zip([0] + split_points,      # w/first char and first pt
                       split_points + [len(s)]) # w/last pt and last char
    splits = [s[a:b] for a, b in split_ranges]
    return splits


@dataclass
class Token:
    text: str
    span: tuple[int, int]
    kind: str
    random: Random
    regex: str | None = None

    def compile_repeats(self) -> str:
        a, b = self.span
        char = self.text[0]
        return rf"{char}{{{b - a}}}"

    def compile_edge(self, pos: str, kind: str) -> str:
        the_format = "^{}" if pos == "first" else "{}$"
        if kind == "word":
            n_parts = len(self.text) // 2
            if n_parts > 1:
                word_parts = split_randomly(self.text, 3, self.random)
                text = word_parts[0] + ".*" + word_parts[2]
            else:
                text = self.text
        else:
            text = self.text
        return the_format.format(rf"{escape(text, ESCAPES)}")

    def compile_simple(self, groups: list[str]) -> str:
        if len(self.text) > 1:
            count = self.random.choice(["+", "*", f"{{{len(self.raw_text)}}}"])
        else:
            count = self.random.choice(["", "*"])
        return rf"{self.random.choice(groups)}{count}"

    def compile_paired(self, delimiters: list[str]) -> str:
        delim_a, delim_b = delimiters
        delim_start, delim_end = self.text.find(delim_a), self.text.find(delim_b, len(delim_a))
        if self.random.randint(0, 1):
            inside = self.text[delim_start+(len(delim_a)):delim_end]
        else:
            inside = ".*"
        return rf"{delim_a}{inside}{delim_b}"

    def compile_word(self):
        if "(" or "[" or ")" or "]" in self.text:
            return
        n_parts = len(self.text) // 3
        if self.random.randint(0, 1) and n_parts > 1:
            word_parts = split_randomly(self.text, n_parts, self.random)
            for i, part in enumerate(word_parts):
                if self.random.randint(0, 1) and len(part) > 1 and not "\\" in part:
                    range_a, range_b = get_alphabet_range(part)
                    word_parts[i] = rf"[{escape(range_a, RANGE_ESCAPES)}-{escape(range_b, RANGE_ESCAPES)}]{{{len(part)}}}"
                elif self.random.randint(0, 1):
                    word_parts[i] = r".*"
                else:
                    pass
            return ''.join(word_parts).replace('.*.*', '.*')
        else:
            return self.text

    def compile_char(self):
        if self.random.randint(0, 2):
            return
        if self.random.randint(0, 1):
            order = ord(self.text)
            a, b = chr(order - self.random.randint(0, 10)),\
                   chr(order + self.random.randint(0, 10))
            if a.isalnum() and b.isalnum():
                return rf"[{a}-{b}]"
            else:
                return r"."
        else:
            return self.text

    def compile_regex(self):
        match self.kind.split("_"):
            # quote, parenthesis
            case ["parenthesis"]:
                self.regex = self.compile_paired(["\\(", "\\)"])
            case ["brackets"]:
                self.regex = self.compile_paired(["\\[", "\\]"])
            case ["quote"]:
                self.regex = self.compile_paired(['"', '"'])
            case ["number"]:
                self.regex = self.compile_simple(["[0-9]", "\\d"])
            case ["repeats"]:
                self.regex = self.compile_repeats()
            case ["space"]:
                self.regex = self.compile_simple(["\\s"])
            case ["first"|"last", _] as args:
                self.regex = self.compile_edge(*args)
            case [_, "word"]:
                self.regex = self.compile_word()
            case [_, "char"] as args:
                return
                self.regex = self.compile_char()
            case _ as x:
                print('?', x)
                self.regex = None

    def __hash__(self):
        return hash(self.regex)

    def __post_init__(self):
        self.raw_text = self.text
        self.text = escape(self.text, ESCAPES)
        self.compile_regex()


def tokenize(s: str, r: Random) -> list[Token]:
    tokens = [[Token(x.group(0), x.span(), group, r) for x in re.finditer(pattern, s)]
              for group, pattern in GROUPS.items()]
    return list(chain(*tokens))


def overlap(a: tuple[int, int], b: tuple[int, int]) -> bool:
    a1, a2, b1, b2 = *a, *b
    return max(a1, b1) < min(a2, b2)


def test_overlap(l: list[Token], t: Token):
    return all([not overlap(x.span, t.span) for x in l])

ALL_TOKENS: dict[str, list[Token]] = {}
def generate(s: str, seed: str, n=20):
    s_hash = hashlib.sha1(s.encode()).hexdigest()
    if not ALL_TOKENS.get(s_hash):
        ALL_TOKENS[s_hash] = [x for x in tokenize(s, Random(seed)) if x.regex]
    r = Random(seed)
    tokens = ALL_TOKENS[s_hash]
    picks = []
    for i in range(n):
        t = r.choice(tokens)
        if t.regex:
            t.regex = t.regex.replace(".*.*", ".*")
            match_test = re.search(t.regex, t.raw_text)
            overlap_test = test_overlap(picks, t)
            if match_test and overlap_test:
                if not "first" in t.kind and r.randint(0, 1):
                    t.regex = ".*" + t.regex
                if not "last" in t.kind and r.randint(0, 1):
                    t.regex = t.regex + ".*"
                t.regex = t.regex.replace(".*.*", ".*")
                picks.append(t)
            else:
                print("NO ", t.regex)
    return list(set(picks))
