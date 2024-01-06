# Streak

## 3.1.0 (2024-01-07)

#### Changed

- The streak file is now stored within `user-emacs-directory` by default. The
  old XDG-based location is still respected if the user hasn't migrated.

## 3.0.0 (2021-10-02)

#### Added

- `streak-remove` to remove streaks that you no longer want to track.

#### Changed

- Streaks files of the very first, non-JSON format can no longer be read.

## 2.1.0 (2021-09-09)

`streak-mode` now supports multiple streaks and totally customizable streak
messages.

#### Added

- `(defun streak-new ...)` to interactively create a new streak.
- `(defcustom streak-formatters ...)` to set per-streak string formatting functions.

#### Changed

- The streak file data format is now JSON. The old format can still be read, but
  will be overwritten the first time you make a change, say via `streak-new` or
  `streak-reset`, etc.
- `streak-reset`, `streak-increment`, and `streak-decrement` now perform auto-completion.
- Hours are no longer shown on the first day.

#### Removed

- The `streak-hour-pattern` and `streak-day-pattern` are removed in favour of `streak-formatters`.

## 2.0.0 (2021-08-22)

#### Added

- A proper package "short description".

#### Changed

- Proper mode (de)activation logic.
- Save streak file in XDG cache.

#### Removed

- A number of internal functions that didn't need to be exposed.

## 1.1.0 (2021-07-20)

#### Changed

- If the streak is less than a day, hours are now shown.

## 1.0.0

Initial release.
