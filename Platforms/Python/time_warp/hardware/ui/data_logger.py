"""
Data logging component for hardware sensor data
Provides CSV export and real-time data storage
"""

import csv
import json
import threading
import time
from collections import deque
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

try:
    import pandas as pd

    HAS_PANDAS = True
except ImportError:
    HAS_PANDAS = False


class DataLogger:
    """Handles logging of sensor data to files and memory"""

    def __init__(self, max_memory_entries: int = 1000):
        self.max_memory_entries = max_memory_entries
        self.data_buffer: Dict[str, deque] = {}
        self.log_directory = Path.home() / ".time_warp" / "hardware_logs"
        self.log_directory.mkdir(parents=True, exist_ok=True)

        # Current session data
        self.session_start = datetime.now()
        self.session_data: List[Dict[str, Any]] = []

    def log_data(self, device_id: str, data: Dict[str, Any]):
        """Log sensor data for a device"""
        timestamp = data.get("timestamp", time.time())

        # Add to memory buffer
        if device_id not in self.data_buffer:
            self.data_buffer[device_id] = deque(maxlen=self.max_memory_entries)

        entry = {"timestamp": timestamp, "device_id": device_id, **data}

        self.data_buffer[device_id].append(entry)
        self.session_data.append(entry)

    def get_recent_data(
        self,
        device_id: str,
        count: int = 100,
    ) -> List[Dict[str, Any]]:
        """Get recent data entries for a device"""
        if device_id not in self.data_buffer:
            return []

        buffer = self.data_buffer[device_id]
        return list(buffer)[-count:]

    def export_to_csv(
        self,
        device_id: str,
        filename: Optional[str] = None,
    ) -> str:
        """Export device data to CSV file"""
        if device_id not in self.data_buffer:
            raise ValueError(f"No data available for device {device_id}")

        if filename is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filename = f"{device_id}_{timestamp}.csv"

        filepath = self.log_directory / filename

        data = list(self.data_buffer[device_id])

        if not data:
            raise ValueError(f"No data to export for device {device_id}")

        # Get all unique keys for CSV headers
        all_keys: Set[str] = set()
        for entry in data:
            all_keys.update(entry.keys())

        fieldnames = sorted(all_keys)

        with open(filepath, "w", newline="", encoding="utf-8") as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(data)

        return str(filepath)

    def export_session_to_csv(self, filename: Optional[str] = None) -> str:
        """Export all session data to CSV"""
        if not self.session_data:
            raise ValueError("No session data to export")

        if filename is None:
            timestamp = self.session_start.strftime("%Y%m%d_%H%M%S")
            filename = f"session_{timestamp}.csv"

        filepath = self.log_directory / filename

        # Get all unique keys
        all_keys: Set[str] = set()
        for entry in self.session_data:
            all_keys.update(entry.keys())

        fieldnames = sorted(all_keys)

        with open(filepath, "w", newline="", encoding="utf-8") as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(self.session_data)

        return str(filepath)

    def export_to_json(
        self,
        device_id: str,
        filename: Optional[str] = None,
    ) -> str:
        """Export device data to JSON file"""
        if device_id not in self.data_buffer:
            raise ValueError(f"No data available for device {device_id}")

        if filename is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filename = f"{device_id}_{timestamp}.json"

        filepath = self.log_directory / filename

        data = list(self.data_buffer[device_id])

        with open(filepath, "w", encoding="utf-8") as jsonfile:
            json.dump(data, jsonfile, indent=2, default=str)

        return str(filepath)

    def get_statistics(
        self,
        device_id: str,
    ) -> Dict[str, Any]:
        """Get statistics for device data"""
        if (
            device_id not in self.data_buffer
            or not self.data_buffer[device_id]
        ):
            return {}

        data = list(self.data_buffer[device_id])

        if not HAS_PANDAS:
            # Basic statistics without pandas
            stats = {
                "total_entries": len(data),
                "time_range": {
                    "start": min(d["timestamp"] for d in data),
                    "end": max(d["timestamp"] for d in data),
                },
            }
            return stats

        # Convert to DataFrame for easy statistics
        df = pd.DataFrame(data)

        stats = {
            "total_entries": len(data),
            "time_range": {
                "start": df["timestamp"].min(),
                "end": df["timestamp"].max(),
            },
        }

        # Statistics for numeric columns
        numeric_columns = df.select_dtypes(include=["number"]).columns
        for col in numeric_columns:
            if col != "timestamp":
                series = df[col]
                stats[col] = {
                    "mean": series.mean(),
                    "min": series.min(),
                    "max": series.max(),
                    "std": series.std(),
                }

        return stats

    def clear_device_data(self, device_id: str):
        """Clear all data for a specific device"""
        if device_id in self.data_buffer:
            self.data_buffer[device_id].clear()

    def clear_all_data(self):
        """Clear all logged data"""
        self.data_buffer.clear()
        self.session_data.clear()
        self.session_start = datetime.now()

    def get_available_devices(self) -> List[str]:
        """Get list of devices with logged data"""
        return list(self.data_buffer.keys())

    def get_log_directory(self) -> str:
        """Get the log directory path"""
        return str(self.log_directory)


class DataLoggerManager:
    """Manages data logging across the application"""

    _instance = None
    _lock = threading.Lock()

    def __new__(cls):
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self):
        if not hasattr(self, "_initialized"):
            self.logger = DataLogger()
            self._initialized = True

    def get_logger(self) -> DataLogger:
        """Get the data logger instance"""
        return self.logger


def get_data_logger() -> DataLogger:
    """Get the global data logger instance"""
    return DataLoggerManager().get_logger()
