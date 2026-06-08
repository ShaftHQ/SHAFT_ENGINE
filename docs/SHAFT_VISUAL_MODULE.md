# SHAFT Visual Module

OpenCV-backed image matching, Applitools Eyes validation, and Selenium Shutterbug comparison are provided by the optional `shaft-visual` module.

Add the BOM and the optional module alongside `shaft-engine`:

```xml
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.shafthq</groupId>
            <artifactId>shaft-bom</artifactId>
            <version>${shaft.version}</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>

<dependencies>
    <dependency>
        <groupId>io.github.shafthq</groupId>
        <artifactId>shaft-engine</artifactId>
    </dependency>
    <dependency>
        <groupId>io.github.shafthq</groupId>
        <artifactId>shaft-visual</artifactId>
    </dependency>
</dependencies>
```

No initialization API is required. `shaft-visual` registers its `VisualProcessingProvider` through Java `ServiceLoader` metadata. Projects that use only ordinary screenshots, Java2D highlighting, animated GIFs, or Healenium can depend on `shaft-engine` alone without downloading OpenCV.
