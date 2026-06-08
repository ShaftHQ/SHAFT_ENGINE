# Pre-modularization cold-cache dependency baseline

Each fixture was compiled against the legacy `io.github.shafthq:SHAFT_ENGINE` coordinate using its own empty temporary Maven repository. The SHAFT JAR and POM are seeded before measurement; repository growth therefore represents downloaded dependencies and build plugins.

| Fixture | Artifacts | Classpath JAR bytes | Repository growth bytes | Elapsed seconds |
| --- | ---: | ---: | ---: | ---: |
| api | 341 | 352,001,986 | 380,333,446 | 67.115 |
| appium-mobile | 341 | 352,001,986 | 380,333,446 | 63.843 |
| browserstack-sdk | 341 | 352,001,986 | 380,333,446 | 75.081 |
| desktop-video | 341 | 352,001,986 | 380,333,446 | 67.478 |
| junit-web | 341 | 352,001,986 | 380,333,446 | 65.871 |
| legacy-coordinate | 341 | 352,001,986 | 380,333,446 | 71.007 |
| opencv-visual | 341 | 352,001,986 | 380,333,446 | 72.197 |
| testng-web | 341 | 352,001,986 | 380,333,446 | 60.863 |

Elapsed time and repository growth are observations, not equality gates. Artifact coordinates, scopes, compressed JAR sizes, and SHA-256 values are compared exactly by `--verify`.
