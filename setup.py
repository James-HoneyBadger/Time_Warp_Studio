"""Setup configuration for Time Warp IDE Python port."""

import os

try:
    from setuptools import find_packages, setup
except ImportError as exc:  # pragma: no cover - packaging guard
    raise SystemExit("setuptools is required to build this package") from exc

# Read README for long description
with open("README.md", "r", encoding="utf-8") as readme_file:
    long_description = readme_file.read()

# Read requirements


def read_requirements(filename):
    """Read requirements from file."""
    if os.path.exists(filename):
        with open(filename, "r", encoding="utf-8") as req_file:
            return [
                line.strip()
                for line in req_file
                if line.strip() and not line.startswith("#")
            ]
    return []


BASE_URL = "https://github.com/James-HoneyBadger/Time_Warp"

# fmt: off
DESCRIPTION = (
    "Educational programming environment for BASIC, PILOT, and Logo"
)
# fmt: on

BUG_TRACKER_URL = "https://github.com/James-HoneyBadger/Time_Warp/issues"
DOCUMENTATION_URL = BASE_URL + "/tree/main/platforms/python"
SOURCE_URL = BASE_URL


setup(
    name="time-warp-ide",
    version="2.0.0",  # Stable release
    author="James Temple",
    author_email="james@honey-badger.org",
    description=DESCRIPTION,
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/James-HoneyBadger/Time_Warp",
    project_urls={
        "Bug Tracker": BUG_TRACKER_URL,
        "Documentation": DOCUMENTATION_URL,
        "Source Code": SOURCE_URL,
    },
    packages=find_packages(exclude=["tests", "tests.*"]),
    python_requires=">=3.8",
    install_requires=[
        # Core has no external dependencies (pure Python)
    ],
    extras_require={
        "gui": [
            "PySide6>=6.5.0",
        ],
        "dev": [
            "pytest>=7.0.0",
            "pytest-cov>=4.0.0",
            "black>=23.0.0",
            "flake8>=6.0.0",
            "mypy>=1.0.0",
        ],
        "all": [
            "PySide6>=6.5.0",
            "pytest>=7.0.0",
            "pytest-cov>=4.0.0",
        ],
    },
    entry_points={
        "console_scripts": [
            # GUI entry point
            "time-warp-ide=time_warp_ide:main",
        ],
    },
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Education",
        "Topic :: Education",
        "Topic :: Software Development :: Interpreters",
        "License :: OSI Approved :: MIT License",  # Adjust as needed
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Operating System :: OS Independent",
    ],
    keywords=("education programming pilot basic logo " "turtle-graphics interpreter"),
    include_package_data=True,
    zip_safe=False,
)
