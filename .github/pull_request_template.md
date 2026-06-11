## 📋 Description

> Summarize your change in one or two sentences. What does this PR do and why is it needed?

<!-- Replace this with your description -->


## 🔗 Related Issue(s)

> Link any related issues (use `Closes #<number>` to auto-close on merge, or `Related to #<number>` for partial fixes).

- Closes #


## 🗂️ Type of Change

> Check all that apply.

- [ ] 🐛 Bug fix (non-breaking change that fixes an issue)
- [ ] 🚀 New feature (non-breaking change that adds functionality)
- [ ] 💥 Breaking change (fix or feature that would cause existing tests to fail)
- [ ] ⚡ Performance improvement
- [ ] 🔒 Security fix
- [ ] 📖 Documentation update
- [ ] ✅ Test coverage improvement
- [ ] 🔧 CI/CD or infrastructure change
- [ ] 📦 Dependency update
- [ ] 🔨 Refactor / code cleanup


## ✅ Pre-Submission Checklist

> Check the items that apply to this change and explain any unavailable
> environment-dependent validation.

- [ ] I have read the [Contributing Guide](../CONTRIBUTING.md) and this PR follows its guidelines.
- [ ] Documentation/agent-only changes pass their deterministic validators and `git diff --check`.
- [ ] Behavior changes have focused automated coverage.
- [ ] Affected tests pass: `mvn -pl shaft-engine -am test -Dtest=<AffectedTestClass>`.
- [ ] Code/build changes compile once with `mvn clean install -DskipTests -Dgpg.skip`.
- [ ] New `public` methods and classes have JavaDoc comments with `@param` / `@return` / `@throws` tags.
- [ ] I have not introduced any hardcoded credentials, secrets, or sensitive data.
- [ ] I have not broken backward compatibility (or documented the breaking change above).


## 🧪 How to Test

> Describe the steps a reviewer can follow to verify this change works as expected.

1. 
2. 
3. 

## Evidence

> Provide the smallest evidence that proves the change: validator output, test
> results, logs, or a concise before/after. Attach screenshots only for visual
> behavior.

<!-- Paste results or attach relevant visual evidence. -->


## 📝 Additional Notes

> Anything else reviewers should know? Tricky logic, known limitations, follow-up work?

<!-- Replace this comment with any additional notes -->
