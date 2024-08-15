Contributing Guide
==================

*Thank you so much for considering contribution to Lighthouse!*

First, please read the Lighthouse :ref:`Code of Conduct`. This project
will not take any contribution coming from those who do not abide by
the code of conduct. This means that if a person is currently under
disciplinary action via avenues set forth in that document, we will
ignore that person's PR and/or any issues they may log.

A contribution can be large or small, code or non-code. To make a
contribution, first log a GitHub issue. Talk about what you want, and
ask for other's opinions.

When you go to make the PR, please use the following checklist to test
whether or not it is likely to be accepted:

1. **Is it based on the ``develop`` branch?** Lighthouse uses the
   `git-flow`_ framework for branch management. Please make PRs to the
   ``develop`` branch.
2. **Do you have tests in your PR, and do they pass?** Tests are in
   two places in lighthouse: the ``test/Lighthouse`` directory, where more
   or less normal unit tests reside; and the
   ``api-test/`` directory, where REST API-level
   tests reside. You must have at least an API-level test as a
   "spot-check" of your feature if the PR is to be merged. The test
   need not be elaborate; simple tests are better than no tests.
3. **Is your PR backwards compatible?** The biggest feature Lighthouse
   provides is backwards compatibility. If Lighthouse breaks a build, it
   is a bug. A PR is herein defined to be "backwards incompatible"
   if 1) it significantly changes the content of any previously merged unit or
   script test and 2) if it breaks any of them.
4. **Did you add documentation around the feature in your PR?**
   Generally this at least means adding something to the :ref:`API
   Reference` document.
5. **Did you add an entry to the Changelog?** This project keeps a
   curated :ref:`changelog <Lighthouse Changelog>`.

There are some exceptions to the above rules. If your patch is less than
two lines' difference from the previous version, your PR may be a "typo" PR,
which may qualify to get around some of the above rules. Just ask the team
on your GitHub issue.

.. _git-flow: http://nvie.com/posts/a-successful-git-branching-model/
