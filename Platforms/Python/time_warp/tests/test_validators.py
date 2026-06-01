"""Tests for time_warp.utils.validators module."""

import pytest
from time_warp.utils.validators import (
    ValidationError,
    validate_arg_count,
    validate_numeric,
    validate_variable_name,
    validate_range,
)


# ---------------------------------------------------------------------------
# ValidationError
# ---------------------------------------------------------------------------


def test_validation_error_is_exception():
    err = ValidationError("test error")
    assert isinstance(err, Exception)
    assert str(err) == "test error"


# ---------------------------------------------------------------------------
# validate_arg_count
# ---------------------------------------------------------------------------


def test_validate_arg_count_exact_match():
    # Should not raise
    validate_arg_count(["a", "b"], 2, "TEST")


def test_validate_arg_count_too_few_raises():
    with pytest.raises(ValidationError):
        validate_arg_count(["a"], 2, "TEST")


def test_validate_arg_count_too_many_raises():
    with pytest.raises(ValidationError):
        validate_arg_count(["a", "b", "c"], 2, "TEST")


def test_validate_arg_count_range_within():
    # Should not raise
    validate_arg_count(["a", "b"], (1, 3), "TEST")


def test_validate_arg_count_range_at_min():
    validate_arg_count(["a"], (1, 3), "TEST")


def test_validate_arg_count_range_at_max():
    validate_arg_count(["a", "b", "c"], (1, 3), "TEST")


def test_validate_arg_count_range_below_min_raises():
    with pytest.raises(ValidationError):
        validate_arg_count([], (1, 3), "TEST")


def test_validate_arg_count_range_above_max_raises():
    with pytest.raises(ValidationError):
        validate_arg_count(["a", "b", "c", "d"], (1, 3), "TEST")


# ---------------------------------------------------------------------------
# validate_numeric
# ---------------------------------------------------------------------------


def test_validate_numeric_integer():
    result = validate_numeric(42)
    assert result == 42


def test_validate_numeric_float():
    result = validate_numeric(3.14)
    assert result == 3.14


def test_validate_numeric_string_integer():
    result = validate_numeric("42")
    assert result == "42"


def test_validate_numeric_string_float():
    result = validate_numeric("3.14")
    assert result == "3.14"


def test_validate_numeric_negative():
    result = validate_numeric(-7)
    assert result == -7


def test_validate_numeric_string_raises():
    with pytest.raises(ValidationError) as exc_info:
        validate_numeric("hello")
    assert "numeric" in str(exc_info.value).lower()


def test_validate_numeric_none_raises():
    with pytest.raises(ValidationError):
        validate_numeric(None)


# ---------------------------------------------------------------------------
# validate_variable_name
# ---------------------------------------------------------------------------


def test_validate_variable_name_simple():
    result = validate_variable_name("myVar")
    assert result == "myVar"


def test_validate_variable_name_with_underscore():
    result = validate_variable_name("_private")
    assert result == "_private"


def test_validate_variable_name_with_numbers():
    result = validate_variable_name("var1")
    assert result == "var1"


def test_validate_variable_name_empty_raises():
    with pytest.raises(ValidationError):
        validate_variable_name("")


def test_validate_variable_name_starts_with_digit_raises():
    with pytest.raises(ValidationError):
        validate_variable_name("123abc")


def test_validate_variable_name_starts_with_digit_only_raises():
    with pytest.raises(ValidationError):
        validate_variable_name("42")


# ---------------------------------------------------------------------------
# validate_range
# ---------------------------------------------------------------------------


def test_validate_range_within():
    result = validate_range(5, 1, 10)
    assert result == 5


def test_validate_range_at_minimum():
    result = validate_range(1, 1, 10)
    assert result == 1


def test_validate_range_at_maximum():
    result = validate_range(10, 1, 10)
    assert result == 10


def test_validate_range_below_minimum_raises():
    with pytest.raises(ValidationError):
        validate_range(0, 1, 10)


def test_validate_range_above_maximum_raises():
    with pytest.raises(ValidationError):
        validate_range(15, 1, 10)


def test_validate_range_negative_values():
    result = validate_range(-5, -10, 0)
    assert result == -5


def test_validate_numeric_integer():
    result = validate_numeric("42")
    assert result == "42" or result == 42


def test_validate_numeric_float():
    result = validate_numeric("3.14")
    assert str(result) == "3.14" or abs(float(result) - 3.14) < 1e-9


def test_validate_numeric_invalid_raises():
    with pytest.raises(ValidationError):
        validate_numeric("abc")


def test_validate_variable_name_valid():
    result = validate_variable_name("myVar")
    assert result == "myVar"


def test_validate_variable_name_invalid_raises():
    with pytest.raises(ValidationError):
        validate_variable_name("123bad")
