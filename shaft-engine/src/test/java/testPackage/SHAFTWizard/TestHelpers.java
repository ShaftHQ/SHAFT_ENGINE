package testPackage.SHAFTWizard;

/**
 * Hermetic test fixtures. The registration form renders its submitted data locally
 * instead of posting to an external site, so E2E runs do not depend on third-party availability.
 */
public class TestHelpers {
    public static String registrationForm = """
            data:text/html,<html>
            <form onsubmit="event.preventDefault(); var data = Array.from(new FormData(this)).map(function (entry) { return entry[0] + '=' + entry[1]; }).join('&'); var header = document.createElement('h2'); header.textContent = 'Submitted Form Data'; var result = document.createElement('div'); result.textContent = data; document.body.replaceChildren(header, result);">
              <div class="container">
                <h1>Register</h1>
                <p>Please fill in this form to create an account.</p>
                <hr>
                        
                <label for="email"><b>Email</b></label>
                <input type="text" placeholder="Enter Email" name="email" id="email" required>
                        
                <label for="psw"><b>Password</b></label>
                <input type="password" placeholder="Enter Password" name="psw" id="psw" required>
                        
                <label for="psw-repeat"><b>Repeat Password</b></label>
                <input type="password" placeholder="Repeat Password" name="psw-repeat" id="psw-repeat" required>
                <hr>                        
                <button type="submit">Register</button>
              </div>
            </form>
            </html>""";
}
