#!/usr/bin/env python3
"""
Script to create GitHub issues from CLAUDE.md issue sections.

Usage:
    python create_github_issues.py [--dry-run]

Requirements:
    pip install PyGithub

Setup:
    1. Create a GitHub personal access token with 'repo' scope
    2. Set environment variable: export GITHUB_TOKEN=your_token_here
    3. Run the script
"""

import re
import os
import sys
from typing import List, Dict, Optional
from dataclasses import dataclass

try:
    from github import Github
    from github.GithubException import GithubException
except ImportError:
    print("Error: PyGithub not installed. Install with: pip install PyGithub")
    sys.exit(1)


@dataclass
class Issue:
    """Represents a parsed issue from CLAUDE.md"""
    number: int
    title: str
    body: str
    status: str  # "COMPLETED", "RESOLVED", "PLANNED"
    labels: List[str]
    is_closed: bool


def parse_claude_md(filepath: str = "CLAUDE.md") -> List[Issue]:
    """Parse CLAUDE.md and extract issue sections."""

    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    issues = []

    # Pattern to match issue headers like "# ISSUE 1: TITLE - STATUS ✅"
    issue_pattern = re.compile(
        r'^# ISSUE (\d+): (.+?) - (COMPLETED|RESOLVED|PLANNED)(?: ✅)?$',
        re.MULTILINE
    )

    matches = list(issue_pattern.finditer(content))

    for i, match in enumerate(matches):
        issue_num = int(match.group(1))
        title = match.group(2).strip()
        status = match.group(3)

        # Extract body content (from current match to next match or end)
        start_pos = match.end()
        end_pos = matches[i + 1].start() if i + 1 < len(matches) else len(content)
        body_content = content[start_pos:end_pos].strip()

        # Clean up body - remove next section headers
        body_lines = []
        for line in body_content.split('\n'):
            # Stop at next major section
            if line.startswith('---') or line.startswith('# PLANNED') or line.startswith('# SYSTEM STATUS'):
                break
            body_lines.append(line)

        body = '\n'.join(body_lines).strip()

        # Truncate if too long (GitHub has limits)
        if len(body) > 60000:
            body = body[:60000] + "\n\n...(truncated - see CLAUDE.md for full details)"

        # Determine labels based on title and status
        labels = []
        title_lower = title.lower()

        if status in ["COMPLETED", "RESOLVED"]:
            labels.append("fixed")

        if any(word in title_lower for word in ["bug", "error", "failure", "fix"]):
            labels.append("bug")
        elif any(word in title_lower for word in ["enhancement", "feature", "support"]):
            labels.append("enhancement")

        if "format" in title_lower or "detection" in title_lower:
            labels.append("data-processing")

        if "test" in title_lower or "baseline" in title_lower:
            labels.append("testing")

        # Default label if none assigned
        if not labels:
            labels.append("documentation")

        is_closed = status in ["COMPLETED", "RESOLVED"]

        issues.append(Issue(
            number=issue_num,
            title=f"Issue {issue_num}: {title}",
            body=body,
            status=status,
            labels=labels,
            is_closed=is_closed
        ))

    return issues


def create_github_issue(repo, issue: Issue, dry_run: bool = False) -> Optional[int]:
    """Create a GitHub issue and return its number."""

    if dry_run:
        print(f"\n{'='*80}")
        print(f"[DRY RUN] Would create issue:")
        print(f"  Title: {issue.title}")
        print(f"  Labels: {', '.join(issue.labels)}")
        print(f"  Status: {'Closed' if issue.is_closed else 'Open'}")
        print(f"  Body preview: {issue.body[:200]}...")
        return None

    try:
        # Create the issue
        gh_issue = repo.create_issue(
            title=issue.title,
            body=issue.body,
            labels=issue.labels
        )

        print(f"✓ Created issue #{gh_issue.number}: {issue.title}")

        # Close if needed
        if issue.is_closed:
            gh_issue.edit(state='closed')
            print(f"  └─ Closed issue #{gh_issue.number}")

        return gh_issue.number

    except GithubException as e:
        print(f"✗ Failed to create issue: {issue.title}")
        print(f"  Error: {e}")
        return None


def main():
    """Main execution function."""

    # Check for dry-run flag
    dry_run = '--dry-run' in sys.argv

    if dry_run:
        print("Running in DRY RUN mode - no issues will be created\n")

    # Get GitHub token
    token = os.environ.get('GITHUB_TOKEN')
    if not token and not dry_run:
        print("Error: GITHUB_TOKEN environment variable not set")
        print("\nTo set it:")
        print("  export GITHUB_TOKEN=your_github_personal_access_token")
        print("\nTo create a token:")
        print("  1. Go to https://github.com/settings/tokens")
        print("  2. Generate new token (classic)")
        print("  3. Select 'repo' scope")
        print("  4. Copy the token and set GITHUB_TOKEN")
        sys.exit(1)

    # Parse CLAUDE.md
    print("Parsing CLAUDE.md...")
    issues = parse_claude_md()
    print(f"Found {len(issues)} issues to create\n")

    if not issues:
        print("No issues found in CLAUDE.md")
        return

    # Show summary
    print(f"Summary:")
    print(f"  Total issues: {len(issues)}")
    print(f"  Closed (resolved): {sum(1 for i in issues if i.is_closed)}")
    print(f"  Open (planned): {sum(1 for i in issues if not i.is_closed)}")
    print()

    if not dry_run:
        # Confirm before proceeding
        response = input(f"Create {len(issues)} GitHub issues? (yes/no): ").strip().lower()
        if response != 'yes':
            print("Aborted.")
            return

    # Connect to GitHub
    if not dry_run:
        print("\nConnecting to GitHub...")
        g = Github(token)

        # Get repository (auto-detect from git remote)
        try:
            import subprocess

            # Try 'origin' first, then fallback to first available remote
            remote_url = None
            for remote_name in ['origin', 'new-remote']:
                result = subprocess.run(
                    ['git', 'remote', 'get-url', remote_name],
                    capture_output=True,
                    text=True
                )
                if result.returncode == 0:
                    remote_url = result.stdout.strip()
                    print(f"Using git remote '{remote_name}': {remote_url}")
                    break

            if not remote_url:
                # Get any available remote
                result = subprocess.run(
                    ['git', 'remote'],
                    capture_output=True,
                    text=True,
                    check=True
                )
                remotes = result.stdout.strip().split('\n')
                if remotes and remotes[0]:
                    result = subprocess.run(
                        ['git', 'remote', 'get-url', remotes[0]],
                        capture_output=True,
                        text=True,
                        check=True
                    )
                    remote_url = result.stdout.strip()
                    print(f"Using git remote '{remotes[0]}': {remote_url}")

            # Parse repo from URL (handle both HTTPS and SSH)
            if remote_url and 'github.com/' in remote_url:
                repo_path = remote_url.split('github.com/')[-1].replace('.git', '')
                print(f"Detected repository: {repo_path}")
                repo = g.get_repo(repo_path)
            else:
                raise ValueError("Could not parse GitHub repository from remote URL")

        except Exception as e:
            print(f"Error: Could not auto-detect repository: {e}")
            print("\nPlease ensure you're in a git repository with GitHub remote")
            sys.exit(1)
    else:
        repo = None

    # Create issues
    print(f"\n{'Creating issues' if not dry_run else 'Preview of issues to create'}:")
    print("="*80)

    created_count = 0
    for issue in issues:
        issue_number = create_github_issue(repo, issue, dry_run)
        if issue_number:
            created_count += 1

    # Summary
    print(f"\n{'='*80}")
    if dry_run:
        print(f"\n[DRY RUN] Would have created {len(issues)} issues")
        print("\nTo actually create the issues, run:")
        print("  python create_github_issues.py")
    else:
        print(f"\nSuccessfully created {created_count}/{len(issues)} issues")
        print(f"\nView issues at: https://github.com/{repo.full_name}/issues")

        # Suggest updating CLAUDE.md
        print("\n" + "="*80)
        print("Next steps:")
        print("1. Review the created issues on GitHub")
        print("2. Update CLAUDE.md to reference GitHub issue numbers:")
        print("   Example: # ISSUE 1: ... (GitHub #42)")


if __name__ == "__main__":
    main()
