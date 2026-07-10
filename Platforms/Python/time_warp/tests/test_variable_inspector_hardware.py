"""Tests for hardware snapshot rendering in the variable inspector."""

from time_warp.ui.variable_inspector import VariableInspector


def test_variable_inspector_formats_hardware_snapshot(qapp):
    inspector = VariableInspector()
    inspector.update_variables(
        {
            "X": 12,
            "__hardware__": {
                "environment": {
                    "temperature": 29.0,
                    "humidity": 55.0,
                    "light_level": 900.0,
                },
                "devices": {
                    "led1": {
                        "type": "led",
                        "active": True,
                        "value": 77,
                        "reading_count": 0,
                    }
                },
            },
        }
    )

    assert inspector.rowCount() == 2
    hardware_text = inspector.item(0, 1).text() if inspector.item(0, 0).text() == "__hardware__" else inspector.item(1, 1).text()
    assert "Hardware snapshot" in hardware_text
    assert "temperature=29.0" in hardware_text
    assert "led1" in hardware_text