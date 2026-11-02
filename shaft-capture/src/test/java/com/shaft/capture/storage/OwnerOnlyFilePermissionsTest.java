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

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
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

    @Test
    void restrictedDirectoryRemainsTraversableAndWritableByItsOwner() throws IOException {
        Path directory = Files.createDirectories(tempDir.resolve("owner-only-dir"));

        OwnerOnlyFilePermissions.restrictDirectoryToOwner(directory);

        // On POSIX, a directory needs its own execute (search) bit for its owner to traverse into
        // it -- restrictToOwner's plain read+write-only permissions would lock the owner out of a
        // directory (unlike a file, where read+write alone is sufficient), so this must use the
        // directory-specific overload and keep the directory usable afterward.
        assertDoesNotThrow(() -> Files.writeString(directory.resolve("inside.txt"), "content"),
                "Restricting a directory to its owner must not also block the owner's own access to it");

        try {
            Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(directory);
            assertEquals(Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.OWNER_EXECUTE), permissions);
        } catch (UnsupportedOperationException posixUnsupported) {
            AclFileAttributeView aclView = Files.getFileAttributeView(directory, AclFileAttributeView.class);
            assertTrue(aclView != null, "Filesystem supports neither POSIX permissions nor ACLs");
            List<AclEntry> acl = aclView.getAcl();
            assertEquals(1, acl.size(), "Expected exactly one owner-only ACL entry, got: " + acl);
        }
    }

    @Test
    void doesNotThrowForAMissingDirectory() {
        // Best-effort by design: a missing path must not throw, since callers treat this as a
        // hardening step applied after the directory was already created.
        assertDoesNotThrow(() -> OwnerOnlyFilePermissions.restrictDirectoryToOwner(tempDir.resolve("does-not-exist")));
    }
}
