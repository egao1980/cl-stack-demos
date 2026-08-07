"""Inspired by sqlalchemy ORM tutorial — User/Note CRUD."""

from __future__ import annotations

from sqlalchemy import ForeignKey, String, create_engine, select
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column, relationship


class Base(DeclarativeBase):
    pass


class User(Base):
    __tablename__ = "demo_users"
    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(String(64))
    email: Mapped[str | None] = mapped_column(String(128), nullable=True)
    notes: Mapped[list[Note]] = relationship(back_populates="user")

    @property
    def label(self) -> str:
        return f"{self.name} <{self.email or ''}>"


class Note(Base):
    __tablename__ = "demo_notes"
    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    user_id: Mapped[int] = mapped_column(ForeignKey("demo_users.id"))
    title: Mapped[str] = mapped_column(String(128))
    body: Mapped[str | None] = mapped_column(String(512), nullable=True)
    user: Mapped[User] = relationship(back_populates="notes")


def main() -> None:
    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    with Session(engine) as session:
        u = User(name="ada", email="a@x")
        session.add(u)
        session.flush()
        n = Note(user_id=u.id, title="hello", body="world")
        session.add(n)
        session.commit()
        found = session.get(User, u.id)
        notes = session.scalars(select(Note).where(Note.user_id == u.id)).all()
        print(f"user={found.label} note={n.title} count={len(notes)}")
        assert found.name == "ada" and len(notes) == 1


if __name__ == "__main__":
    main()
