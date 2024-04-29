"""Test the CLI with input-output pairs."""

import subprocess


def cli(*args: tuple[str, str]):
    """Test the CLI with the given input-output pairs."""

    process = subprocess.Popen(
        ["./cli"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
    )

    assert process.stdin is not None
    assert process.stdout is not None

    for test_input, test_output in args:
        process.stdin.write(f"{test_input}\n")
        process.stdin.flush()

        stdout = process.stdout.readline()
        process.stdout.flush()
        assert stdout == f"{test_output}\n"

    process.communicate()


REMEMBER = "I will remember that."


def test_implication():
    """Test the CLI with implication rules."""

    cli(
        ("alice is human", REMEMBER),
        ("every human is mortal", REMEMBER),
        ("is alice mortal", "alice is mortal"),
        (
            "explain why alice is mortal",
            "alice is human, every human is mortal, therefore alice is mortal",
        ),
        ("is alice not mortal", "alice is mortal"),
        (
            "explain why alice is not mortal",
            "alice is human, every human is mortal, therefore alice is mortal",
        ),
    )


def test_negation():
    """Test the CLI with negation rules."""

    cli(
        ("every teacher is happy", REMEMBER),
        ("donald is not happy", REMEMBER),
        ("is donald a teacher", "donald is not a teacher"),
        (
            "explain why donald is a teacher",
            "donald is not happy, every teacher is happy, therefore donald is not a teacher",
        ),
        ("is donald not a teacher", "donald is not a teacher"),
        (
            "explain why donald is not a teacher",
            "donald is not happy, every teacher is happy, therefore donald is not a teacher",
        ),
    )


def test_negation_simple():
    """Test the CLI with negation rules."""

    cli(
        ("pixie is not blue", REMEMBER),
        ("is pixie blue", "pixie is not blue"),
    )


def test_disjunction():
    """Test the CLI with disjunction rules."""

    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red or blue", REMEMBER),
        ("pixie is red", REMEMBER),
        ("is pixie blue", "pixie is not blue"),
        (
            "explain why pixie is blue",
            "every pixel is red or is blue, pixie is a pixel, pixie is red, therefore pixie is not blue",
        ),
        ("is pixie not blue", "pixie is not blue"),
        (
            "explain why pixie is not blue",
            "every pixel is red or is blue, pixie is a pixel, pixie is red, therefore pixie is not blue",
        ),
    )

    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red or blue", REMEMBER),
        ("pixie is not red", REMEMBER),
        ("is pixie blue", "pixie is blue"),
        (
            "explain why pixie is blue",
            "every pixel is red or is blue, pixie is a pixel, pixie is not red, therefore pixie is blue",
        ),
        ("is pixie not blue", "pixie is blue"),
        (
            "explain why pixie is not blue",
            "every pixel is red or is blue, pixie is a pixel, pixie is not red, therefore pixie is blue",
        ),
    )


def test_disjunction_three_term():
    """Test the CLI with disjunction rules."""
    # Question is middle term
    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red green or blue", REMEMBER),
        ("pixie is not red", REMEMBER),
        ("pixie is not blue", REMEMBER),
        ("is pixie green", "pixie is green"),
        (
            "explain why pixie is green",
            "every pixel is red is green or is blue, pixie is a pixel, pixie is not red, pixie is not blue, therefore pixie is green",
        ),
    )
    # Question is first term
    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red green or blue", REMEMBER),
        ("pixie is not green", REMEMBER),
        ("pixie is not blue", REMEMBER),
        ("is pixie red", "pixie is red"),
        (
            "explain why pixie is red",
            "every pixel is red is green or is blue, pixie is a pixel, pixie is not green, pixie is not blue, therefore pixie is red",
        ),
    )

    # Question is last term
    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red green or blue", REMEMBER),
        ("pixie is not red", REMEMBER),
        ("pixie is not green", REMEMBER),
        ("is pixie blue", "pixie is blue"),
        (
            "explain why pixie is blue",
            "every pixel is red is green or is blue, pixie is a pixel, pixie is not red, pixie is not green, therefore pixie is blue",
        ),
    )


def test_disjunctive_questions():
    cli(
        ("pixie is red", REMEMBER),
        ("is pixie red or blue", "pixie is red or is blue"),
        (
            "explain why pixie is red or blue",
            "pixie is red, therefore pixie is red or is blue",
        ),
        ("is pixie blue or red", "pixie is blue or is red"),
        (
            "explain why pixie is blue or red",
            "pixie is red, therefore pixie is blue or is red",
        ),
    )
