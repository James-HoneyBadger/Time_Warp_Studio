"""Setup configuration for Time Warp IDE Python port."""

from setuptools import setup, find_packages
import os

# Read README for long description
with open('README.md', 'r', encoding='utf-8') as f:
    long_description = f.read()

# Read requirements


def read_requirements(filename):
    """Read requirements from file."""
    if os.path.exists(filename):
        with open(filename, 'r') as f:
            return [
                line.strip()
                for line in f
                if line.strip() and not line.startswith('#')
            ]
    return []


setup(
    name='time-warp-ide',
    version='2.0.0',  # Stable release
    author='James Temple',
    author_email='james@honey-badger.org',
    description=(
        'Educational TempleCode programming environment '
        '(unified BASIC, PILOT, Logo)'
    ),
    long_description=long_description,
    long_description_content_type='text/markdown',
    url='https://github.com/James-HoneyBadger/Time_Warp',
    project_urls={
        'Bug Tracker': (
            'https://github.com/James-HoneyBadger/Time_Warp/issues'
        ),
        'Documentation': (
            'https://github.com/James-HoneyBadger/Time_Warp/tree/main/'
            'Time_Warp_Python'
        ),
        'Source Code': 'https://github.com/James-HoneyBadger/Time_Warp',
    },
    packages=find_packages(exclude=['tests', 'tests.*']),
    python_requires='>=3.8',
    install_requires=[
        # Core has no external dependencies (pure Python)
    ],
    extras_require={
        'gui': [
            'PySide6>=6.5.0',
        ],
        'dev': [
            'pytest>=7.0.0',
            'pytest-cov>=4.0.0',
            'black>=23.0.0',
            'flake8>=6.0.0',
            'mypy>=1.0.0',
        ],
        'all': [
            'PySide6>=6.5.0',
            'pytest>=7.0.0',
            'pytest-cov>=4.0.0',
        ],
    },
    entry_points={
        'console_scripts': [
            # CLI entry point (when implemented)
            'time-warp=time_warp.cli:main',
        ],
    },
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Education',
        'Topic :: Education',
        'Topic :: Software Development :: Interpreters',
        'License :: OSI Approved :: MIT License',  # Adjust as needed
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Programming Language :: Python :: 3.12',
        'Operating System :: OS Independent',
    ],
    keywords=(
        'education programming templecode pilot basic logo '
        'turtle-graphics interpreter'
    ),
    include_package_data=True,
    zip_safe=False,
)
