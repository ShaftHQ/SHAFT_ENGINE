package com.shaft.capture.storage;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OwnerOnlyFilePermissionsTest {

    @TempDir
    Path tempDir;

    @Test
    void restrictsFileToOwnerOnlyOnWhicheverAttributeViewThisFilesystemSupports() throws IOException {
        Path file = Files.writeString(tempDir.resolve("secret.txt"), "sensitive");

        OwnerOnlyFilePermissions.restrictToOwner(file);

        try {
            Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(file);
            assertEquals(Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE), permissions);
        } catch (UnsupportedOperationException posixUnsupported) {
            AclFileAttributeView aclView = Files.getFileAttributeView(file, AclFileAttributeView.class);
            assertTrue(aclView != null, "Filesystem supports neither POSIX permissions nor ACLs");
            List<AclEntry> acl = aclView.getAcl();
            assertEquals(1, acl.size(), "Expected exactly one owner-only ACL entry, got: " + acl);
        }
    }

    @Test
    void doesNotThrowForAMissingFile() {
        // Best-effort by design: a missing path must not throw, since callers treat this as a
        // hardening step applied after a real write already succeeded.
        OwnerOnlyFilePermissions.restrictToOwner(tempDir.resolve("does-not-exist.txt"));
    }
}
