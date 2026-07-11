# Changelog

This file records all notable changes to this project.

This changelog uses the [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) format.
This project uses prerelease versions such as 0.1.0-alpha.

## [Unreleased]

## [0.24.0] - 2026-07-11

### Changed

- Update bundled `FSharp.Core` from 10.0.101 to 10.1.301 for F# 10.1 assembly support.

## [0.23.0] - 2025-12-30

### Performance

- Optimize current property extraction to reduce string operations.
- Optimize window beginning cursor calculation with binary search.
- Avoid unnecessary state updates when refresh is not required.

## [0.22.0] - 2025-11-30

### Changed

- Update the optimized binary target from .NET 6.0 to .NET 8.0.
- Use the .NET 8.0 binary on PowerShell 7.4 and later.
- Use the .NET Standard 2.0 binary on older PowerShell versions.

### Fixed

- Fix multi-byte line rendering to measure display width accurately.

### Performance

- Add query caching to reduce repeated query preparation work.

## [0.22.0-alpha] - 2025-11-30

### Added

- Add multi-target module publishing with a script module loader.

### Changed

- Add .NET 6.0 and .NET Standard 2.0 builds to the module package.

### Fixed

- Fix compatibility with Windows PowerShell 5.1.
- Fix screen line appending to respect the current window width.

## [0.21.0] - 2025-11-30

### Fixed

- Fix invalid regular expression matching so invalid match patterns no longer return empty results.
- Fix dictionary entry matching when values are null.
- Fix query parsing so empty property tokens do not consume the next token.
- Fix property collection so pocof skips properties of processed types.

### Performance

- Optimize key handling and key reading to reduce allocations while preserving responsiveness.
- Replace quotation-based predicate generation with direct predicate builders.
- Improve query execution speed and memory usage.
- Optimize query parsing and tokenization to reduce intermediate allocations.
- Optimize screen rendering, query strings, information display, and padding strings.
- Optimize cursor movement, word actions, property suggestions, and property lookup paths.

## [0.20.0] - 2025-09-22

### Changed

- Sync `Select-Pocof` keybinding documentation with the implementation.

### Fixed

- Fix module import failure when Windows PowerShell cannot find the module manifest.
- Fix rendering loop termination on empty render events.

### Performance

- Optimize screen rendering and screen line writing.
- Optimize key input reading and key input conversion.
- Optimize query parsing, word actions, and query-related string handling to reduce allocations.

## [0.19.0] - 2025-09-22

### Fixed

- Fix case-insensitive property filtering and property completion behavior.
- Fix access to platform-dependent properties.

### Performance

- Optimize property access for `DictionaryEntry` and `PSObject`.

## [0.18.1] - 2024-12-27

### Fixed

- Fix query window width calculation to resolve a UI rendering issue.

## [0.18.0] - 2024-12-27

### Changed

- Add validation for non-null cmdlet options.
- Update help metadata and help URI.
- Switch `InputObject` and `Query` parameter positions for improved usability.

### Fixed

- Fix documentation links.

## [0.17.0] - 2024-11-16

### Added

- Add `WordDelimiters` parameter for configuring word navigation and editing boundaries.
- Add word navigation, word deletion, and word selection actions.
- Add `Ctrl` + arrow key combinations and `Ctrl` + `Shift` combinations for word operations.

### Fixed

- Fix Windows PowerShell compatibility in module name resolution.
- Correct the default keymap for word selection actions.
- Treat zero-character selection as input mode.

### Performance

- Optimize word cursor functions.

## [0.16.1] - 2024-10-05

### Fixed

- Handle empty render stacks for .NET Framework forward compatibility.

## [0.16.0] - 2024-10-05

### Fixed

- Fix cancellation during record processing.

## [0.15.0] - 2024-09-06

### Changed

- Remove the `None` operator from supported query operators.
- Keep property candidate listing case-insensitive in case-sensitive matching mode.

### Fixed

- Fix dynamic operator handling for null values.
- Fix initial window width handling.

### Performance

- Optimize matcher and query processing.
- Add parallel query processing.

## [0.14.1] - 2024-07-14

### Fixed

- Fix an incorrect total count display.

## [0.14.0] - 2024-07-07

### Added

- Add periodic rendering during record processing.
- Add automatic query window resizing during cmdlet record processing.

### Changed

- Start rendering and query input after launch.

### Fixed

- Fix rendering with `NonInteractive`.
- Fix property collection and property map updates.
- Fix screen cleanup when exiting during record processing.

### Performance

- Improve record processing responsiveness with reactive rendering and asynchronous querying.
- Preserve input order for unique entries while using concurrent input storage.

## [0.13.0] - 2024-05-18

### Added

- Add `Unique` option to return unique entries.

## [0.12.1] - 2024-05-12

### Performance

- Optimize accumulation of input objects and properties.

## [0.12.0] - 2024-05-03

### Changed

- Change the default keymap for `DeleteForwardInput` and `DeleteBackwardInput`.

### Performance

- Avoid high CPU load while waiting for input.

## [0.11.0] - 2024-04-01

### Added

- Add query string selection support.
- Add action handlers for selecting all query text and deleting selected text.

### Changed

- Add a documentation tip for default parameter settings.
- Hide the cursor while rendering.

### Fixed

- Prevent unhandled errors when `Ctrl+S` pauses output.
- Correct displayed operator names from `notcmatch` and `notclike` to `cnotmatch` and `cnotlike`.
- Fix query selection rendering and action handling.

## [0.10.0] - 2024-02-25

### Added

- Add new `Layout` option values, including half-height layouts.
- Add accepted values and default values to `Select-Pocof` parameter documentation.

### Changed

- Normalize `Matcher` and `Operator` value casing.

### Fixed

- Fix `Prefix` matching.
- Fix `TopDownHalf` screen rendering corruption.
- Fix filtering of `DateTime` values with local date patterns.
- Fix filtering with localized string representations.
- Fix screen refresh bugs.

## [0.9.0] - 2024-01-28

### Changed

- Add compatible PowerShell editions and platform tags to the module manifest.

### Fixed

- Fix screen cleanup behavior.

### Performance

- Reduce time complexity in query string rendering.

## [0.8.0] - 2024-01-14

### Added

- Add query window support.
- Add query selection actions.
- Add full-width character support for the query window.

### Fixed

- Fix filtered count updates.
- Fix query window rendering and cursor rendering during console resizing.
- Fix query window behavior when moving to the beginning or end of a line.
- Fix cursor position after property completion and query condition changes.

## [0.7.0] - 2023-12-30

### Added

- Add bottom-up layout support.
- Add property completion and property completion rotation.
- Add keymap validation at cmdlet startup.
- Add online help URL.

### Changed

- Change target framework to .NET Standard 2.0 for broader PowerShell compatibility.

### Fixed

- Fix property search, property list display, and property completion behavior.
- Fix double completion when the cursor is in the middle of a keyword.
- Fix missing key handlers.
- Improve Linux compatibility.

### Performance

- Improve paste handling performance.
- Optimize screen refresh to occur when needed.

## [0.6.0-alpha] - 2023-08-20

### Changed

- Reduce unused DLLs bundled in the module package.

## [0.5.0-alpha] - 2023-08-16

### Added

- Add support for overriding the default keymap.

### Changed

- Replace PowerShellGet-based publishing with PSResourceGet.

### Fixed

- Fix unexpected hashtable behavior.
- Fix query editing at the beginning of the line.
- Fix input handling while pressing the `Shift` modifier.

## [0.4.0-alpha] - 2023-02-26

### Added

- Add property filtering for input objects.
- Add property suggestions in the notification area.
- Add `SuppressProperties` option and key operation to toggle suggested property visibility.

### Changed

- Support `PSObject` and `DictionaryEntry` values in the internal entry list.

### Fixed

- Find properties case-insensitively.
- Avoid exceptions from dynamic property lookup.
- Return all entries when using an empty query with inverted filtering.
- Cancel when the user types `Ctrl+C`.
- Prevent insertion of control characters and undefined key combinations into the query.

## [0.3.0-alpha] - 2022-11-27

### Added

- Add composite query `or`, `and`, and `none` modes.

### Changed

- Display all candidates for an empty `like` query.

## [0.2.0-alpha] - 2022-10-16

### Added

- Add hashtable support by filtering hashtable keys and values.

## [0.1.1-alpha] - 2022-09-25

### Fixed

- Fix output handling to write each selected entry individually.

## [0.1.0-alpha] - 2022-09-10

### Added

- Add initial `Select-Pocof` cmdlet implementation.
- Add interactive filtering display and keyboard actions.
- Add simple match, wildcard, and case-sensitive query support.
- Add `NonInteractive` option.
- Add PowerShell table-style output formatting.
- Add initial cmdlet documentation.

### Fixed

- Show invalid regular expression errors in the information area.

### Notes

- This is the initial alpha release of `pocof`.

---

[Unreleased]: https://github.com/krymtkts/pocof/compare/v0.23.0...HEAD
[0.23.0]: https://github.com/krymtkts/pocof/compare/v0.22.0...v0.23.0
[0.22.0]: https://github.com/krymtkts/pocof/compare/v0.22.0-alpha...v0.22.0
[0.22.0-alpha]: https://github.com/krymtkts/pocof/compare/v0.21.0...v0.22.0-alpha
[0.21.0]: https://github.com/krymtkts/pocof/compare/v0.20.0...v0.21.0
[0.20.0]: https://github.com/krymtkts/pocof/compare/v0.19.0...v0.20.0
[0.19.0]: https://github.com/krymtkts/pocof/compare/v0.18.1...v0.19.0
[0.18.1]: https://github.com/krymtkts/pocof/compare/v0.18.0...v0.18.1
[0.18.0]: https://github.com/krymtkts/pocof/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/krymtkts/pocof/compare/v0.16.1...v0.17.0
[0.16.1]: https://github.com/krymtkts/pocof/compare/v0.16.0...v0.16.1
[0.16.0]: https://github.com/krymtkts/pocof/compare/v0.15.0...v0.16.0
[0.15.0]: https://github.com/krymtkts/pocof/compare/v0.14.1...v0.15.0
[0.14.1]: https://github.com/krymtkts/pocof/compare/v0.14.0...v0.14.1
[0.14.0]: https://github.com/krymtkts/pocof/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/krymtkts/pocof/compare/v0.12.1...v0.13.0
[0.12.1]: https://github.com/krymtkts/pocof/compare/v0.12.0...v0.12.1
[0.12.0]: https://github.com/krymtkts/pocof/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/krymtkts/pocof/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/krymtkts/pocof/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/krymtkts/pocof/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/krymtkts/pocof/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/krymtkts/pocof/compare/v0.6.0-alpha...v0.7.0
[0.6.0-alpha]: https://github.com/krymtkts/pocof/compare/v0.5.0-alpha...v0.6.0-alpha
[0.5.0-alpha]: https://github.com/krymtkts/pocof/compare/v0.4.0-alpha...v0.5.0-alpha
[0.4.0-alpha]: https://github.com/krymtkts/pocof/compare/v0.3.0-alpha...v0.4.0-alpha
[0.3.0-alpha]: https://github.com/krymtkts/pocof/compare/v0.2.0-alpha...v0.3.0-alpha
[0.2.0-alpha]: https://github.com/krymtkts/pocof/compare/v0.1.1-alpha...v0.2.0-alpha
[0.1.1-alpha]: https://github.com/krymtkts/pocof/compare/v0.1.0-alpha...v0.1.1-alpha
[0.1.0-alpha]: https://github.com/krymtkts/pocof/releases/tag/v0.1.0-alpha
