"""Inspired by pallets/click examples/naval — nested ship/mine groups."""

from __future__ import annotations

import click


@click.group()
@click.version_option()
def cli() -> None:
    """Naval Fate (click naval slim port)."""


@cli.group()
def ship() -> None:
    """Manages ships."""


@ship.command("new")
@click.argument("name")
def ship_new(name: str) -> None:
    """Creates a new ship."""
    click.echo(f"Created ship {name}")


@ship.command("move")
@click.argument("ship_name")
@click.argument("x", type=float)
@click.argument("y", type=float)
@click.option("--speed", default=10, help="Speed in knots.")
def ship_move(ship_name: str, x: float, y: float, speed: int) -> None:
    """Moves SHIP to the new location X,Y."""
    click.echo(f"Moving ship {ship_name} to {x},{y} with speed {speed}")


@cli.group()
def mine() -> None:
    """Manages mines."""


@mine.command("set")
@click.argument("x", type=float)
@click.argument("y", type=float)
def mine_set(x: float, y: float) -> None:
    """Sets a mine at a specific coordinate."""
    click.echo(f"Set mine at {x},{y}")


def main() -> None:
    cli.main(["ship", "new", "Destiny"], standalone_mode=False)
    cli.main(["ship", "move", "Destiny", "10", "20", "--speed", "15"], standalone_mode=False)
    cli.main(["mine", "set", "5", "6"], standalone_mode=False)


if __name__ == "__main__":
    main()
