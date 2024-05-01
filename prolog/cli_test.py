# pylint: disable=missing-function-docstring, missing-module-docstring, missing-class-docstring

import subprocess


class Cli:
    Remember = "I will remember that."

    def __init__(self):
        self.process = subprocess.Popen(
            ["./cli"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
        )

    def __call__(self, value: str):
        assert self.process.stdin is not None
        assert self.process.stdout is not None

        self.process.stdout.flush()
        self.process.stdin.write(f"{value}\n")
        self.process.stdin.flush()
        return self.process.stdout.readline().rstrip("\n")

    def __del__(self):
        assert self.process.stdin is not None
        assert self.process.stdout is not None

        self("stop")
        self.process.terminate()


def test_implication():
    cli = Cli()
    assert cli("alice is human") == Cli.Remember
    assert cli("every human is mortal") == Cli.Remember
    assert cli("is alice mortal") == "alice is mortal"
    assert (
        cli("explain why alice is mortal")
        == "alice is human, every human is mortal, therefore alice is mortal"
    )
    assert cli("is alice not mortal") == "alice is mortal"
    assert (
        cli("explain why alice is not mortal")
        == "alice is human, every human is mortal, therefore alice is mortal"
    )


def test_negation():
    cli = Cli()
    assert cli("every teacher is happy") == Cli.Remember
    assert cli("donald is not happy") == Cli.Remember
    assert cli("is donald a teacher") == "donald is not a teacher"
    explanation = (
        "donald is not happy, every teacher is happy, therefore donald is not a teacher"
    )
    assert cli("explain why donald is a teacher") == explanation
    assert cli("is donald not a teacher") == "donald is not a teacher"
    assert cli("explain why donald is not a teacher") == explanation


def test_negation_simple():
    cli = Cli()
    assert cli("pixie is not blue") == Cli.Remember
    assert cli("is pixie blue") == "pixie is not blue"
    assert cli("is pixie not blue") == "pixie is not blue"

    cli = Cli()
    assert cli("alice does not fly") == Cli.Remember
    assert cli("does alice fly") == "alice does not fly"
    assert cli("does alice not fly") == "alice does not fly"


def test_disjunction():
    cli = Cli()
    assert cli("pixie is a pixel") == Cli.Remember
    assert cli("every pixel is red or blue") == Cli.Remember
    assert cli("pixie is red") == Cli.Remember
    assert cli("is pixie blue") == "pixie is not blue"
    assert (
        cli("explain why pixie is blue")
        == "every pixel is red or is blue, pixie is a pixel, pixie is red, therefore pixie is not blue"
    )
    assert cli("is pixie not blue") == "pixie is not blue"
    assert (
        cli("explain why pixie is not blue")
        == "every pixel is red or is blue, pixie is a pixel, pixie is red, therefore pixie is not blue"
    )

    cli = Cli()
    assert cli("pixie is a pixel") == Cli.Remember
    assert cli("every pixel is red or blue") == Cli.Remember
    assert cli("pixie is not red") == Cli.Remember
    assert cli("is pixie blue") == "pixie is blue"
    assert (
        cli("explain why pixie is blue")
        == "every pixel is red or is blue, pixie is a pixel, pixie is not red, therefore pixie is blue"
    )
    assert cli("is pixie not blue") == "pixie is blue"
    assert (
        cli("explain why pixie is not blue")
        == "every pixel is red or is blue, pixie is a pixel, pixie is not red, therefore pixie is blue"
    )


def test_disjunction_three_term():
    # Question is middle term
    cli = Cli()
    assert cli("pixie is a pixel") == Cli.Remember
    assert cli("every pixel is red green or blue") == Cli.Remember
    assert cli("pixie is not red") == Cli.Remember
    assert cli("pixie is not blue") == Cli.Remember
    assert cli("is pixie green") == "pixie is green"
    assert (
        cli("explain why pixie is green")
        == "every pixel is red is green or is blue, pixie is a pixel, pixie is not red, pixie is not blue, therefore pixie is green"
    )

    # Question is first term
    cli = Cli()
    assert cli("pixie is a pixel") == Cli.Remember
    assert cli("every pixel is red green or blue") == Cli.Remember
    assert cli("pixie is not green") == Cli.Remember
    assert cli("pixie is not blue") == Cli.Remember
    assert cli("is pixie red") == "pixie is red"
    assert (
        cli("explain why pixie is red")
        == "every pixel is red is green or is blue, pixie is a pixel, pixie is not green, pixie is not blue, therefore pixie is red"
    )

    # Question is last term
    cli = Cli()
    assert cli("pixie is a pixel") == Cli.Remember
    assert cli("every pixel is red green or blue") == Cli.Remember
    assert cli("pixie is not red") == Cli.Remember
    assert cli("pixie is not green") == Cli.Remember
    # TODO: this fails
    assert cli("is pixie blue") == "pixie is blue"
    assert (
        cli("explain why pixie is blue")
        == "every pixel is red is green or is blue, pixie is a pixel, pixie is not red, pixie is not green, therefore pixie is blue"
    )


def test_disjunctive_questions():
    cli = Cli()
    assert cli("pixie is red") == Cli.Remember
    assert cli("is pixie red or blue") == "pixie is red or is blue"
    assert (
        cli("explain why pixie is red or blue")
        == "pixie is red, therefore pixie is red or is blue"
    )
    assert cli("is pixie blue or red") == "pixie is blue or is red"
    assert (
        cli("explain why pixie is blue or red")
        == "pixie is red, therefore pixie is blue or is red"
    )


def test_conjunction():
    cli = Cli()
    assert cli("bob is human and mortal") == Cli.Remember
    assert cli("is bob human") == "bob is human"
    assert cli("is bob mortal") == "bob is mortal"


def test_conjunction_three_term():
    # Question is middle term
    cli = Cli()
    assert cli("bob is human mortal and does not fly") == Cli.Remember
    assert cli("is bob human") == "bob is human"
    assert cli("is bob mortal") == "bob is mortal"

    # TODO: this fails
    assert cli("does bob fly") == "bob does not fly"


def test_conjunction_questions():
    cli = Cli()
    assert cli("bob is human mortal and does not fly") == Cli.Remember
    assert cli("is bob mortal and human") == "bob is mortal and is human"


def test_default():
    cli = Cli()
    assert cli("most birds fly") == Cli.Remember
    assert cli("alice is a bird") == Cli.Remember
    assert cli("does alice fly") == "it is likely that alice flies"
    assert (
        cli("explain why alice flies")
        == "alice is a bird, most birds fly, therefore it is likely that alice flies"
    )


def test_existential():
    """Test the CLI with existential rules."""
    cli(
        ("alice is a genius", REMEMBER),
        ("some geniuses win", REMEMBER),
        ("does alice win", "it could be that alice wins"),
        (
            "explain why it could be that alice wins",
            "alice is a genius, some geniuses win, therefore it could be that alice wins",
        ),
    )
    cli(
        ("some humans are geniuses", REMEMBER),
        ("all geniuses win", REMEMBER),
        ("do humans win", "some humans win"),
        (
            "explain why some humans win",
            "some humans are geniuses, every genius wins, therefore some humans win",
        ),
    )
