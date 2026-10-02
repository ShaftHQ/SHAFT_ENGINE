package com.shaft.intellij.properties;

import com.intellij.lang.properties.IProperty;
import com.intellij.lang.properties.psi.PropertiesFile;
import com.intellij.psi.PsiFile;
import com.shaft.intellij.project.ShaftProjectDetector;

/** Decides which {@code .properties} files get SHAFT completion and validation (issue #6417). */
final class ShaftPropertiesFiles {
    private ShaftPropertiesFiles() {
    }

    /**
     * SHAFT applies to a properties file in a SHAFT project, or to any properties file that
     * already sets a SHAFT key (a SHAFT {@code custom.properties} copied elsewhere).
     */
    static boolean applies(PsiFile file) {
        if (!(file instanceof PropertiesFile propertiesFile)) {
            return false;
        }
        if (ShaftProjectDetector.isShaftProject(file.getProject())) {
            return true;
        }
        for (IProperty property : propertiesFile.getProperties()) {
            String key = property.getUnescapedKey();
            if (key != null && ShaftPropertyCatalog.find(key) != null) {
                return true;
            }
        }
        return false;
    }
}
