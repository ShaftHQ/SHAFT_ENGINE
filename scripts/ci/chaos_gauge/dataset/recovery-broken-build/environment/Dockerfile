FROM python:3.12.11-slim@sha256:47ae396f09c1303b8653019811a8498470603d7ffefc29cb07c88f1f8cb3d19f

RUN useradd --create-home --uid 10001 --shell /bin/bash chaosgauge
WORKDIR /app
COPY --chown=chaosgauge:chaosgauge . /app
ENV HOME=/home/chaosgauge
USER chaosgauge
