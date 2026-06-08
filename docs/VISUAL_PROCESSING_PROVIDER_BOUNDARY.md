# Visual-processing provider boundary

Issue [#2816](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/2816) introduces the boundary needed to move optional computer-vision behavior out of `shaft-engine` without moving screenshot capture or Healenium integration.

## Operation inventory

| `ImageProcessingActions` operation | Classification | Boundary decision |
| --- | --- | --- |
| `compareImageFolders` | JDK image/file comparison | Remains in `shaft-engine`; uses `ImageIO` and image data buffers. |
| `highlightElementInScreenshot` | Core screenshot decoration | Remains in `shaft-engine`; implemented with JDK `BufferedImage`/`Graphics2D`, so ordinary screenshots do not initialize OpenCV. |
| `findImageWithinCurrentPage` | Optional computer vision | Delegates to the discovered `VisualProcessingProvider`. |
| `formatElementLocatorToImagePath` | Core baseline naming | Remains in `shaft-engine`. |
| `getReferenceImage` | Core baseline file access | Remains in `shaft-engine`. |
| `getShutterbugDifferencesImage` | Core baseline file access | Remains in `shaft-engine` for backward compatibility. |
| `compareAgainstBaseline` / `EXACT_OPENCV` | Optional computer vision | Delegates to the discovered provider. |
| `compareAgainstBaseline` / Shutterbug and Applitools engines | Third-party visual integrations | Delegates to the discovered provider. |
| `loadOpenCV` | Backward-compatible OpenCV entry point | Delegates provider loading and no longer embeds OpenCV types in the core class. |

`ScreenshotManager`, Selenium screenshot capture, and Healenium integration remain outside this provider boundary.

## Discovery contract

Providers implement `VisualProcessingProvider` and are discovered with `ServiceLoader`. Discovery sorts implementations by class name and rejects multiple providers with a deterministic error instead of choosing based on classpath order. The OpenCV implementation and its service descriptor are packaged in `io.github.shafthq:shaft-visual`, so adding that artifact enables OpenCV, Applitools Eyes, and Shutterbug behavior without a new initialization API.

When no provider is installed, provider-dependent operations report the Maven coordinate `io.github.shafthq:shaft-visual`. Core screenshot capture, JDK highlighting, animated GIF generation, baseline file handling, Healenium, and non-OpenCV assertions remain in `shaft-engine` and do not perform provider discovery.
