package com.shaft.capture.storage;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclEntryPermission;
import java.nio.file.attribute.AclEntryType;
import java.nio.file.attribute.AclFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.UserPrincipal;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Restricts a file or directory to owner-only access on any filesystem. Extracted from
 * {@code com.shaft.capture.network.SecretHeaderReplacer} so every locally-stored secret --
 * captured header secrets, and the mobile capture proxy's CA private key -- goes through the same
 * single, tested implementation of this security-sensitive operation rather than duplicating it.
 */
public final class OwnerOnlyFilePermissions {

    private OwnerOnlyFilePermissions() {
    }

    /**
     * Sets owner-only permissions on a file.
     * On POSIX systems, uses {@link PosixFilePermission} (0600 = owner read+write only).
     * On Windows (or any non-POSIX filesystem that supports ACLs), replaces the ACL with a single
     * ALLOW entry for the owner, denying access to every other principal.
     *
     * @param path file path
     */
    public static void restrictToOwner(Path path) {
        restrict(path, false);
    }

    /**
     * Sets owner-only permissions on a directory, keeping it traversable by its owner.
     * Unlike {@link #restrictToOwner(Path)}, this also grants the owner execute (search)
     * permission -- on POSIX systems, a directory without its own execute bit cannot be
     * traversed into by anyone, including its owner, so a plain owner-read/write-only directory
     * would block every subsequent read or write of files inside it.
     *
     * @param directory directory path
     */
    public static void restrictDirectoryToOwner(Path directory) {
        restrict(directory, true);
    }

    private static void restrict(Path path, boolean directory) {
        try {
            Set<PosixFilePermission> perms = new HashSet<>();
            perms.add(PosixFilePermission.OWNER_READ);
            perms.add(PosixFilePermission.OWNER_WRITE);
            if (directory) {
                perms.add(PosixFilePermission.OWNER_EXECUTE);
            }
            Files.setPosixFilePermissions(path, perms);
        } catch (UnsupportedOperationException posixUnsupported) {
            // Not a POSIX filesystem (e.g. Windows/NTFS); fall back to an owner-only ACL.
            applyOwnerOnlyAcl(path, directory);
        } catch (IOException ignored) {
            // Best-effort; the file remains at its filesystem-default permissions.
        }
    }

    private static void applyOwnerOnlyAcl(Path path, boolean directory) {
        try {
            AclFileAttributeView aclView = Files.getFileAttributeView(path, AclFileAttributeView.class);
            if (aclView == null) {
                // Neither POSIX nor ACL views are supported on this filesystem; nothing more to do.
                return;
            }
            UserPrincipal owner = aclView.getOwner();
            Set<AclEntryPermission> permissions = new HashSet<>(Set.of(
                    AclEntryPermission.READ_DATA,
                    AclEntryPermission.WRITE_DATA,
                    AclEntryPermission.APPEND_DATA,
                    AclEntryPermission.READ_ATTRIBUTES,
                    AclEntryPermission.WRITE_ATTRIBUTES,
                    AclEntryPermission.READ_NAMED_ATTRS,
                    AclEntryPermission.WRITE_NAMED_ATTRS,
                    AclEntryPermission.READ_ACL,
                    AclEntryPermission.WRITE_ACL,
                    AclEntryPermission.DELETE,
                    AclEntryPermission.SYNCHRONIZE));
            if (directory) {
                // Mirrors the POSIX execute bit above: lets the owner traverse into the directory.
                permissions.add(AclEntryPermission.EXECUTE);
            }
            AclEntry ownerEntry = AclEntry.newBuilder()
                    .setType(AclEntryType.ALLOW)
                    .setPrincipal(owner)
                    .setPermissions(permissions)
                    .build();
            aclView.setAcl(List.of(ownerEntry));
        } catch (IOException | UnsupportedOperationException ignored) {
            // Best-effort; owner-only ACL could not be applied on this filesystem.
        }
    }
}
