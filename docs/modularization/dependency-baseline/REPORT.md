# Modular cold-cache dependency baseline

Each fixture was compiled against its declared SHAFT module using an empty temporary Maven repository. The reactor artifacts and POMs are seeded before measurement; repository growth therefore represents downloaded dependencies and build plugins.

| Fixture | Artifacts | Classpath JAR bytes | Repository growth bytes | Elapsed seconds |
| --- | ---: | ---: | ---: | ---: |
| api | 329 | 284,731,691 | 312,843,433 | 67.838 |
| appium-mobile | 329 | 284,731,691 | 312,843,433 | 66.955 |
| bom | 333 | 286,667,824 | 310,129,676 | 41.254 |
| browserstack-sdk | 331 | 322,940,699 | 351,088,061 | 72.772 |
| desktop-video | 342 | 313,850,103 | 342,074,693 | 76.184 |
| junit-web | 329 | 284,731,691 | 312,843,433 | 75.297 |
| legacy-coordinate | 329 | 284,731,691 | 312,843,433 | 69.006 |
| opencv-visual | 329 | 284,731,691 | 312,843,433 | 79.584 |
| testng-web | 329 | 284,731,691 | 312,843,433 | 73.035 |

Elapsed time and repository growth are observations, not equality gates. Artifact coordinates, scopes, compressed JAR sizes, and SHA-256 values are compared exactly by `--verify`.
