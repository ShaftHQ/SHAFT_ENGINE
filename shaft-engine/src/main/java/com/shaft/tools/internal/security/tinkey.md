Tink is an open source library by Google that facilitates encryption/decryption
[GitHub - Google/Tink](https://github.com/google/tink)

These are the basic business requirements:

- All test data under the test data directory (already a property) should be encrypted manually by the user using the
  CMD tool. We should provide the user with a bat file that they can double-click, and it will encrypt all their data
  for
  them recursively. This is done outside the engine.
- All test data under the test data directory (already a property) should be decrypted automatically and recursively by
  SHAFT before any test execution.
- To automatically decrypt the data, SHAFT will accept some properties from the user to show the name of the key and any
  other parameters required by tinkey.
- This should be implemented to support both local keys and remote keys as per TinKey's documentation.
- All decrypted test data should be deleted automatically by SHAFT after any execution to ensure that only the encrypted
  files exist in SHAFT's test data directory.
- This concludes the initial business requirements, with area for future enhancements.

Implementation approach:

1. work on the coded part
    1. adding the maven dependency
    2. adding the code to automatically decrypt data using local key
    3. adding the needed properties to enable automated decryption
    4. adding support for using remote keys
    5. adding the code to delete the decrypted files after execution is finished
2. work on the tinkey binary facilitation part
    1. understanding how the binary works to generate new keys
    2. understanding how the binary works to encrypt files
    3. creating a .bat file that automatically encrypts all files in the testData directory
    4. bundling the bat/sh file with SHAFT to be generated for all projects

I will keep this document updated with my findings to document the entire thought process and implementation sequence.

Implementation steps:

- found the needed data here [Dependency](https://github.com/google/tink#getting-started)
- added it to the pom.xml
- found a java guide here [JAVA Todo](https://github.com/google/tink/blob/master/docs/JAVA-HOWTO.md)
- found that extra dependencies are needed for remote key management, added them to the pom.xml
- created a dedicated package and moved this file to under com.shaft.tools.security
- created a demo java class (GoogleTink.java) to start playing around with the implementation
- created a demo test class (src/test/java/testPackage.tink/demo.java) to call the encrypt method from and see how it
  works
- first attempt was successful and the file was encrypted as expected
- first attempt to decrypt the file was a failure, the file was wiped
- restoring from backup
- adding code to load from existing keyset
- pausing the first coding session
  <br/>
- starting the second session
  To use Tinkey you need a key. there are two ways to work through
  this, [the more secure](https://developers.google.com/tink/generate-encrypted-keyset)
  and [the less secure](https://developers.google.com/tink/generate-plaintext-keyset).
  SHAFT will support both, because this is a key generation method and this can be achieved by the user running the
  single command in the above URL.
- to implement I will work with the less secure method, the plaintext keyset (local key file)
- downloading the latest tinkey binary was easy from
  this [link](https://github.com/google/tink/blob/master/docs/TINKEY.md#install-from-prebuilt-binaries)
- extracted the binaries and failed to generate the key using the provided command
- reformatted it to "./tinkey create-keyset --key-template AES128_GCM --out-format json --out aead_keyset.json", ran it
  from powershell, and it worked
- a key.json file was generated successfully
- refactoring the code in GoogleTink.java to accept the path to the key file **WARNING: the key json file must never be
  shared over source control** but even if it was, generating a new key is a piece of cake
- attempting to run with the new algorithm
- data encrypted successfully
- data decrypted successfully
  <br/>

### MILESTONE 1 REACHED || Initial Implementation Success

- creating property file to manage the two needed external variables
- TODO: add logs / report steps
- TODO: create loop to automatically and recursively encrypt/decrypt all the test data subdirectories/files
- testing encryption/decryption flows after migrating to properties
- TESTS PASSED
- implementing logging and reporting (Note to self: do not over-do it, keep all private data off of the logs and
  reports, and don't attach any files in the execution report.)
- implementing loop to automatically and recursively encrypt/decrypt all the test data subdirectories/files
- resolving issues with existing FileActions methods to facilitate reading files recursively
- creating tests to encryptAll and decryptAll test data
- TESTS PASSED

### MILESTONE 2 REACHED || Extended Implementation Success

- TODO: integrate with SHAFT listeners to decrypt before all and encrypt after all seamlessly
- done for the three execution modes (TestNG, Cucumber Native, Cucumber Test Runner)

### MILESTONE 3 REACHED || All Requirements Implemented Successfully with several enhancements

Current user Experience:

1. User will follow the instructions under tinkey.properties file to create a keyset (plaintext or cloud encrypted via
   KMS)
2. User will add the two needed properties in this file
3. SHAFT_Engine will decrypt the data before execution (if encrypted)
4. Execution will behave as expected
5. SHAFT_Engine will encrypt the data after execution
6. No manual user interaction required