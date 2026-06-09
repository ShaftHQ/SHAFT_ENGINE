# Modular cold-cache dependency baseline

Each fixture was compiled against its declared SHAFT module using an empty temporary Maven repository. The reactor artifacts and POMs are seeded before measurement; repository growth therefore represents downloaded dependencies and build plugins.

| Fixture | Artifacts | Classpath JAR bytes | Repository growth bytes | Elapsed seconds |
| --- | ---: | ---: | ---: | ---: |
| api | 308 | 169,941,464 | 197,369,199 | 61.304 |
| appium-mobile | 308 | 169,941,464 | 197,369,199 | 63.201 |
| bom | 334 | 286,750,752 | 310,207,843 | 48.687 |
| browserstack-sdk | 310 | 208,151,248 | 235,613,827 | 63.801 |
| combined-modules | 345 | 353,844,292 | 381,372,591 | 44.794 |
| desktop-video | 318 | 198,872,925 | 226,402,674 | 58.400 |
| junit-web | 308 | 169,941,464 | 197,369,199 | 59.694 |
| legacy-coordinate | 308 | 169,941,464 | 197,369,199 | 65.667 |
| opencv-visual | 330 | 284,527,061 | 312,634,039 | 69.092 |
| testng-web | 308 | 169,941,464 | 197,369,199 | 61.174 |

Elapsed time and repository growth are observations, not equality gates. Artifact coordinates, scopes, compressed JAR sizes, and SHA-256 values are compared exactly by `--verify`.
