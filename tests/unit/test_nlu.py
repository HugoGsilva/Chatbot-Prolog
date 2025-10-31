import pytest

from app.nlu import normalize_actor_name, normalize_genre_name


@pytest.mark.parametrize(
    "input_name, expected",
    [
        ("penelope guiness", "PENELOPE GUINESS"),
        ("PENELOPE GUINESS", "PENELOPE GUINESS"),
        ("pEnElOpE gUiNnEsS", "PENELOPE GUINNESS"),
    ],
)
def test_normalize_actor_name_cases(input_name: str, expected: str) -> None:
    assert normalize_actor_name(input_name) == expected


@pytest.mark.parametrize(
    "input_genre, expected",
    [
        ("action", "Action"),
        ("COMEDY", "Comedy"),
        ("hOrRoR", "Horror"),
        # Casos de borda conhecidos da estratégia .capitalize()
        ("sci-fi", "Sci-fi"),
        ("new wave", "New wave"),
    ],
)
def test_normalize_genre_name_cases(input_genre: str, expected: str) -> None:
    assert normalize_genre_name(input_genre) == expected