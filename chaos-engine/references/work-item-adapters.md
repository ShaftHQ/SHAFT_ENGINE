# Work-item adapters

Adapters only. The [work-item contract](work-item.md) stays SCM-agnostic.
Select the adapter from the detected CLI; never assume a provider.

## GitHub (`gh`) — live CLI

Only live create/search/edit path in this repository today.

| Action | Command shape |
| --- | --- |
| List/search | `gh issue list --repo OWNER/REPO --state STATE --search QUERY --limit 1000 --json url` |
| Create | `gh issue create --repo OWNER/REPO --title TITLE --body BODY --label LABELS` |
| Edit labels | `gh issue edit N --repo OWNER/REPO --add-label L --remove-label L` |
| View | `gh issue view N --repo OWNER/REPO --json labels` |

URL shape: `https://github.com/OWNER/REPO/issues/N`

## GitLab (`glab`) — documented / fake-runner

| Action | Command shape |
| --- | --- |
| Create | `glab issue create --repo GROUP/PROJ --title TITLE --description BODY --label LABELS` |

Builder: `scripts.agents.issue_filing.build_glab_issue_create_argv`.
URL shape: `https://gitlab.com/GROUP/PROJ` issue pages ending at `issues/N`.

Live GitLab network calls are out of scope; tests use fake runners only.

## Azure Boards (`az boards`) — documented / fake-runner

| Action | Command shape |
| --- | --- |
| Create | `az boards work-item create --organization ORG_URL --project PROJECT --title TITLE --type Issue --description BODY` |

Builder: `scripts.agents.issue_filing.build_az_boards_create_argv`.
URL shape: `https://dev.azure.com/ORG/PROJECT` or `ORG.visualstudio.com` work-item edit pages ending at a numeric id.

Live Azure network calls are out of scope; tests use fake runners only.

## Selection

1. Prefer the CLI already authenticated for the adopter repo.
2. Keep GitHub as the only live path until another CLI is wired end-to-end.
3. Still accept non-GitHub https dependency URLs for blocked items.
