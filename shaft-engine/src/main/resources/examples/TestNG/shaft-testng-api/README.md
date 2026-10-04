# SHAFT API example (TestNG)

API tests for the [REST Countries](https://restcountries.com) v5 API, built with
[SHAFT Engine](https://github.com/ShaftHQ/SHAFT_ENGINE) and TestNG.

`TestClass` looks up a country by its capital (`capitals/Cairo`) and asserts that
`data.objects[0].names.common` equals `expectedCountryName` from `simpleJSON.json`.

> REST Countries v3.1 is deprecated and now returns an error envelope, so this
> example targets v5 (`https://api.restcountries.com/countries/v5/`), which needs an
> API key. See the [v3.1 deprecation notice](https://restcountries.com/docs/countries/legacy-api-deprecation).

## Prerequisites

- JDK 25
- Maven 3.9+
- A REST Countries API key. Get one from your account at https://restcountries.com.

## Set up the API key

The v5 API needs a bearer token. The token is kept out of the source code and out of Git.
`RestCountriesCredentials` looks for it in this order:

1. The `restCountries.apiKey` property: a `-D` system property, or a value in any
   `.properties` file under `src/main/resources/properties/` (SHAFT loads them all).
2. The `RESTCOUNTRIES_API_KEY` environment variable.

If the key isn't found, the test fails straight away with a message pointing to this README.

### Local: secrets.properties

1. Copy the template in `src/main/resources/properties/`:

   ```bash
   cp src/main/resources/properties/secrets.properties.example src/main/resources/properties/secrets.properties
   ```

   On Windows PowerShell:
   `Copy-Item src/main/resources/properties/secrets.properties.example src/main/resources/properties/secrets.properties`

2. Open `secrets.properties` and replace the placeholder with your key:

   ```properties
   restCountries.apiKey=rc_live_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   ```

   - No quotes are needed.
   - Don't include the word `Bearer`; the test adds it.

3. Don't commit `secrets.properties`. It's already listed in `.gitignore`. Don't put the key in
   `custom.properties` either, because that file is committed.

### One-off run: -D property

```bash
mvn clean test "-DrestCountries.apiKey=rc_live_xxxxxxxx"
```

### Environment variable

```bash
# macOS / Linux / Git Bash
export RESTCOUNTRIES_API_KEY=rc_live_xxxxxxxx
```

```powershell
# Windows PowerShell
$env:RESTCOUNTRIES_API_KEY = "rc_live_xxxxxxxx"
```

### GitHub Actions

If you generated the project with the GitHub Actions workflow, `.github/workflows/api.yml`
passes a repository secret to the tests as the `RESTCOUNTRIES_API_KEY` environment variable:

1. In your GitHub repository, go to **Settings → Secrets and variables → Actions**.
2. Click **New repository secret**.
3. Name: `RESTCOUNTRIES_API_KEY`. Value: your key.

## Run the tests

```bash
mvn clean test
```

Run the tests from the project root so SHAFT finds `src/main/resources/properties/`. In
IntelliJ, the run configuration's working directory is the project root by default.

## Project layout

```
src/main/resources/properties/
  custom.properties                   SHAFT settings (committed, no secrets)
  secrets.properties.example          Template for your local secrets.properties (committed)
  secrets.properties                  Your real API key (git-ignored, create it yourself)
src/test/java/testPackage/
  TestClass.java                      v5 test, bearer-token auth
  RestCountriesCredentials.java       Resolves the API key from properties or environment
src/test/resources/testDataFiles/
  simpleJSON.json                     Expected values (expectedCountryName)
```
