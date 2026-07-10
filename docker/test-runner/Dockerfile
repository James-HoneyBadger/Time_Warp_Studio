FROM python:3.11-slim

WORKDIR /work

# Install build deps and Qt runtime libraries for PySide6 headless/offscreen tests.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential \
        libglib2.0-0 \
        libfontconfig1 \
        libegl1 \
        libgl1 \
        libxkbcommon0 \
        libdbus-1-3 \
        libxcb1 \
        libx11-6 \
        libxcb-cursor0 \
        libxcb-icccm4 \
        libxcb-keysyms1 \
        libxcb-shape0 \
        libxcb-xfixes0 && \
    rm -rf /var/lib/apt/lists/*

# Copy project into container
COPY . /work

# Upgrade pip and install project dependencies (core + dev)
RUN python -m pip install --upgrade pip setuptools wheel && \
    if [ -f Platforms/Python/requirements.txt ]; then pip install -r Platforms/Python/requirements.txt || true; fi && \
    if [ -f Platforms/Python/requirements-dev.txt ]; then pip install -r Platforms/Python/requirements-dev.txt; fi

ENV PYTHONPATH=/work/Platforms/Python

CMD ["pytest", "-q", "Platforms/Python/time_warp/tests"]
