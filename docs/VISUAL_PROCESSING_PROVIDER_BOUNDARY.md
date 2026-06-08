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
| `compareAgainstBaseline` / `EXACT_OPENCV` | Optional computer vision | Reuses `findImageWithinCurrentPage`, and therefore requires a provider. |
| `compareAgainstBaseline` / Shutterbug and Applitools engines | Third-party visual integrations | Retained unchanged for this preparatory issue; they can move behind the same SHAFT-owned boundary during artifact extraction. |
| `loadOpenCV` | Backward-compatible OpenCV entry point | Delegates provider loading and no longer embeds OpenCV types in the core class. |

`ScreenshotManager`, Selenium screenshot capture, and Healenium integration remain outside this provider boundary.

## Discovery contract

Providers implement `VisualProcessingProvider` and are discovered with `ServiceLoader`. Discovery sorts implementations by class name and rejects multiple providers with a deterministic error instead of choosing based on classpath order. The current OpenCV implementation is registered as a service so behavior remains available before artifact extraction.

When no provider is installed, provider-dependent operations report the future Maven coordinate `io.github.shafthq:shaft-visual`. Core screenshot capture, JDK highlighting, baseline file handling, and non-OpenCV assertions do not perform provider discovery.
