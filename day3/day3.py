def str_to_chars(s: str) -> set[str]:
    ls = [x for x in s]
    return set(ls)


def char_priority(c: str) -> int:
    if c >= 'a' and c <= 'z':
        return ord(c) - ord('a') + 1
    else:
        return ord(c) - ord('A') + 27


class Rucksack:
    first: set[str]
    second: set[str]
    items: set[str]

    def __init__(self, line) -> None:
        comp_len = len(line) // 2
        first_str = line[:comp_len]
        second_str = line[comp_len:]

        self.items = str_to_chars(line)
        self.first = str_to_chars(first_str)
        self.second = str_to_chars(second_str)

    def common_items(self) -> set[str]:
        return self.first & self.second

    def priority(self) -> int:
        return sum(map(char_priority, self.common_items()))


filename = 'input1.txt'

with open(filename) as f:
    lines = f.readlines()
    lines = [l[:-1] for l in lines]

rucksacks = [Rucksack(l) for l in lines]
priority_sum = sum(map(Rucksack.priority, rucksacks))
print(priority_sum)

badge_sum = 0
for i in range(0, len(rucksacks), 3):
    rs1 = rucksacks[i]
    rs2 = rucksacks[i + 1]
    rs3 = rucksacks[i + 2]
    com = rs1.items & rs2.items & rs3.items
    badge_sum += sum(map(char_priority, com))
print(badge_sum)
