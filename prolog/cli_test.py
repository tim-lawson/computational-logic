# pylint: disable=missing-function-docstring,missing-module-docstring,missing-class-docstring

import os
import subprocess


path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "cli")


class Cli:
    Remember = "I will remember that."

    def __init__(self):
        self.process = subprocess.Popen(
            [path], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
        )

    def __call__(self, value: str):
        assert self.process.stdin is not None
        assert self.process.stdout is not None

        self.process.stdout.flush()
        self.process.stdin.write(f"{value}\n")
        self.process.stdin.flush()
        return self.process.stdout.readline().rstrip("\n")

    def test(self, value: str, expected: str):
        assert self(value) == expected

    def test_fact(self, value: str):
        self.test(value, self.Remember)

    def __del__(self):
        assert self.process.stdin is not None
        assert self.process.stdout is not None

        self("stop")
        self.process.terminate()


def test_implication():
    cli = Cli()
    cli.test_fact("alice is human")
    cli.test_fact("every human is mortal")
    cli.test("is alice mortal", "alice is mortal")
    cli.test(
        "explain why alice is mortal",
        "alice is human, every human is mortal, therefore alice is mortal",
    )
    cli.test("is alice not mortal", "alice is mortal")
    cli.test(
        "explain why alice is not mortal",
        "alice is human, every human is mortal, therefore alice is mortal",
    )


def test_negation_simple():
    cli = Cli()
    cli.test_fact("pixie is not blue")
    cli.test("is pixie blue", "pixie is not blue")
    cli.test("is pixie not blue", "pixie is not blue")

    cli = Cli()
    cli.test_fact("alice does not fly")
    cli.test("does alice fly", "alice does not fly")
    cli.test("does alice not fly", "alice does not fly")


def test_negation_modus_tollens():
    cli = Cli()
    cli.test_fact("every teacher is happy")
    cli.test_fact("donald is not happy")
    cli.test("is donald a teacher", "donald is not a teacher")
    cli.test(
        "explain why donald is a teacher",
        "donald is not happy, every teacher is happy, therefore donald is not a teacher",
    )
    cli.test("is donald not a teacher", "donald is not a teacher")
    cli.test(
        "explain why donald is not a teacher",
        "donald is not happy, every teacher is happy, therefore donald is not a teacher",
    )


def test_disjunction_two():
    cli = Cli()
    cli.test_fact("pixie is a pixel")
    cli.test_fact("every pixel is red or blue")
    cli.test_fact("pixie is red")
    cli.test("is pixie blue", "pixie is not blue")
    cli.test(
        "explain why pixie is blue",
        "every pixel is red or is blue, pixie is a pixel, pixie is red, therefore pixie is not blue",
    )
    cli.test("is pixie not blue", "pixie is not blue")
    cli.test(
        "explain why pixie is not blue",
        "every pixel is red or is blue, pixie is a pixel, pixie is red, therefore pixie is not blue",
    )

    cli = Cli()
    cli.test_fact("pixie is a pixel")
    cli.test_fact("every pixel is red or blue")
    cli.test_fact("pixie is not red")
    cli.test("is pixie blue", "pixie is blue")
    cli.test(
        "explain why pixie is blue",
        "every pixel is red or is blue, pixie is a pixel, pixie is not red, therefore pixie is blue",
    )
    cli.test("is pixie not blue", "pixie is blue")
    cli.test(
        "explain why pixie is not blue",
        "every pixel is red or is blue, pixie is a pixel, pixie is not red, therefore pixie is blue",
    )


def test_disjunction_three():
    # The question is the first term.
    cli = Cli()
    cli.test_fact("pixie is a pixel")
    cli.test_fact("every pixel is red green or blue")
    cli.test_fact("pixie is not green")
    cli.test_fact("pixie is not blue")
    cli.test("is pixie red", "pixie is red")
    cli.test(
        "explain why pixie is red",
        "every pixel is red is green or is blue, pixie is a pixel, pixie is not green, pixie is not blue, therefore pixie is red",
    )

    # The question is the middle term.
    cli = Cli()
    cli.test_fact("pixie is a pixel")
    cli.test_fact("every pixel is red green or blue")
    cli.test_fact("pixie is not red")
    cli.test_fact("pixie is not blue")
    cli.test("is pixie green", "pixie is green")
    cli.test(
        "explain why pixie is green",
        "every pixel is red is green or is blue, pixie is a pixel, pixie is not red, pixie is not blue, therefore pixie is green",
    )

    # The question is the last term.
    cli = Cli()
    cli.test_fact("pixie is a pixel")
    cli.test_fact("every pixel is red green or blue")
    cli.test_fact("pixie is not red")
    cli.test_fact("pixie is not green")

    # TODO
    # cli.test("is pixie blue", "pixie is blue")
    # cli.test(
    #     "explain why pixie is blue",
    #     "every pixel is red is green or is blue, pixie is a pixel, pixie is not red, pixie is not green, therefore pixie is blue",
    # )


def test_disjunctive_questions():
    cli = Cli()
    cli.test_fact("pixie is red")
    cli.test("is pixie red or blue", "pixie is red or is blue")
    cli.test(
        "explain why pixie is red or blue",
        "pixie is red, therefore pixie is red or is blue",
    )
    cli.test("is pixie blue or red", "pixie is blue or is red")
    cli.test(
        "explain why pixie is blue or red",
        "pixie is red, therefore pixie is blue or is red",
    )


def test_conjunction_two():
    cli = Cli()
    cli.test_fact("bob is human and mortal")
    cli.test("is bob human", "bob is human")
    cli.test("is bob mortal", "bob is mortal")


def test_conjunction_three():
    # The question is the middle term.
    cli = Cli()
    cli.test_fact("bob is human mortal and does not fly")
    cli.test("is bob human", "bob is human")
    cli.test("is bob mortal", "bob is mortal")

    # TODO
    # cli.test("does bob fly", "bob does not fly")


def test_conjunction_questions():
    cli = Cli()
    cli.test_fact("bob is human mortal and does not fly")
    cli.test("is bob mortal and human", "bob is mortal and is human")


def test_default_reasoning():
    cli = Cli()
    cli.test_fact("most birds fly")
    cli.test_fact("alice is a bird")
    cli.test("does alice fly", "it is likely that alice flies")
    cli.test(
        "explain why it is likely that alice flies",
        "alice is a bird, most birds fly, therefore it is likely that alice flies",
    )


def test_existential_quantification():
    cli = Cli()
    cli.test_fact("alice is a genius")
    cli.test_fact("some geniuses win")
    cli.test("does alice win", "it could be that alice wins")
    cli.test(
        "explain why it could be that alice wins",
        "alice is a genius, some geniuses win, therefore it could be that alice wins",
    )

    cli = Cli()
    cli.test_fact("some humans are geniuses")
    cli.test_fact("every genius wins")
    cli.test("do humans win", "some humans win")
    cli.test(
        "explain why some humans win",
        "some humans are geniuses, every genius wins, therefore some humans win",
    )
