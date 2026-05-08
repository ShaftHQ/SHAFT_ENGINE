package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class TinkeyTests {
    String keysetFilename;
    String kmsServerType;
    String kmsCredentialPath;
    String kmsMasterKeyUri;


    @BeforeClass
    public void beforeClass() {
        keysetFilename = SHAFT.Properties.tinkey.keysetFilename();
        kmsServerType = SHAFT.Properties.tinkey.kmsServerType();
        kmsCredentialPath = SHAFT.Properties.tinkey.kmsCredentialPath();
        kmsMasterKeyUri = SHAFT.Properties.tinkey.kmsMasterKeyUri();

    }

    @Test
    public void test() {
        SHAFT.Properties.tinkey.set().keysetFilename(keysetFilename);
        SHAFT.Properties.tinkey.set().kmsServerType(kmsServerType);
        SHAFT.Properties.tinkey.set().kmsCredentialPath(kmsCredentialPath);
        SHAFT.Properties.tinkey.set().kmsMasterKeyUri(kmsMasterKeyUri);

    }

}
