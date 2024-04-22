import subprocess


def cli(*args: tuple[str, str]):
    """Test the CLI with the given input-output pairs."""

    process = subprocess.Popen(
        ["./cli"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
    )

    assert process.stdin is not None
    assert process.stdout is not None

    for input, output in args:
        process.stdin.write(f"{input}\n")
        process.stdin.flush()

        stdout = process.stdout.readline()
        process.stdout.flush()
        assert stdout == f"{output}\n"

    process.communicate()


REMEMBER = "I will remember that."


def test_implication():
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
    cli(
        ("every teacher is happy", REMEMBER),
        ("donald is not happy", REMEMBER),
        ("is donald a teacher", "donald is not a teacher"),
    )


def test_disjunction():
    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red or blue", REMEMBER),
        ("pixie is red", REMEMBER),
        ("is pixie blue", "pixie is not blue"),
        (
            "explain why pixie is blue",
            "every pixel is red or blue, pixie is a pixel, \
pixie is red, therefore pixie is not blue",
        ),
        ("is pixie not blue", "pixie is not blue"),
        (
            "explain why pixie is not blue",
            "every pixel is red or blue, pixie is a pixel, \
pixie is red, therefore pixie is not blue",
        ),
    )

    cli(
        ("pixie is a pixel", REMEMBER),
        ("every pixel is red or blue", REMEMBER),
        ("pixie is not red", REMEMBER),
        ("is pixie blue", "pixie is blue"),
        (
            "explain why pixie is blue",
            "every pixel is red or blue, pixie is a pixel, \
pixie is not red, therefore pixie is blue",
        ),
        ("is pixie not blue", "pixie is blue"),
        (
            "explain why pixie is not blue",
            "every pixel is red or blue, pixie is a pixel, \
pixie is not red, therefore pixie is blue",
        ),
    )
