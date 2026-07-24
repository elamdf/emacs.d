on splitText(t, delim)
	set oldDelims to AppleScript's text item delimiters
	set AppleScript's text item delimiters to delim
	set parts to text items of t
	set AppleScript's text item delimiters to oldDelims
	return parts
end splitText

on reminderKey(r)
	tell application "Reminders"
		try
			set b to body of r as text
			set firstLine to paragraph 1 of b
			if firstLine begins with "orgsync:" then
				return text 9 thru -1 of firstLine
			end if
		end try
	end tell
	return missing value
end reminderKey

on indexOf(xs, needle)
	repeat with i from 1 to count of xs
		if item i of xs is needle then
			return i
		end if
	end repeat
	return 0
end indexOf

on findReminderById(targetList, reminderId)
	tell application "Reminders"
		repeat with r in reminders of targetList
			if id of r is reminderId then
				return r
			end if
		end repeat
	end tell
	return missing value
end findReminderById

on run argv

	if (count of argv) < 2 then
		error "usage: osascript sync_todos_to_reminders.applescript REMINDERS_LIST TSV_PATH"
	end if

	set reminderListName to item 1 of argv
	set tsvPath to item 2 of argv

	set rowsText to read POSIX file tsvPath as «class utf8»

	tell application "Reminders"
		if not (exists list reminderListName) then
			make new list with properties {name:reminderListName}
		end if

		set targetList to list reminderListName
		set createdCount to 0
		set updatedCount to 0
		set completedCount to 0
		set skippedDuplicateCount to 0
		set existingKeys to {}
		set existingReminderIds to {}
		set seenInputKeys to {}

		repeat with r in reminders of targetList
			set existingKey to my reminderKey(r)
				if existingKey is not missing value and existingKey is not "" then
					if my indexOf(existingKeys, existingKey) is 0 then
						set end of existingKeys to existingKey
						set end of existingReminderIds to id of r
					end if
				end if
			end repeat

		repeat with row in paragraphs of rowsText
			set rowText to row as text
			if rowText is not "" then
				set fields to my splitText(rowText, tab)
				if (count of fields) ≥ 4 then
					set keyText to item 1 of fields
					set idText to item 2 of fields
					set titleText to item 3 of fields
					set sourceText to item 4 of fields
					set noteText to "orgsync:" & keyText & linefeed & "org-id: " & idText & linefeed & "source: " & sourceText

					if my indexOf(seenInputKeys, keyText) is not 0 then
						set skippedDuplicateCount to skippedDuplicateCount + 1
					else
						set end of seenInputKeys to keyText

							set foundReminder to missing value
							set existingIndex to my indexOf(existingKeys, keyText)
							if existingIndex is not 0 then
								set existingReminderId to item existingIndex of existingReminderIds
								set foundReminder to my findReminderById(targetList, existingReminderId)
							end if

							if foundReminder is missing value then
								set newReminder to make new reminder at end of reminders of targetList with properties {name:titleText, body:noteText}
								set end of existingKeys to keyText
								set end of existingReminderIds to id of newReminder
								set createdCount to createdCount + 1
							else
								if name of foundReminder is not titleText then
									set name of foundReminder to titleText
									set updatedCount to updatedCount + 1
								end if
								if body of foundReminder is not noteText then
									set body of foundReminder to noteText
								end if
								if completed of foundReminder is true then
									set completed of foundReminder to false
									set updatedCount to updatedCount + 1
								end if
							end if
						end if
					end if
			end if
		end repeat

		repeat with i from (count of existingKeys) to 1 by -1
			if my indexOf(seenInputKeys, item i of existingKeys) is 0 then
				set staleReminderId to item i of existingReminderIds
				set staleReminder to my findReminderById(targetList, staleReminderId)
				if staleReminder is not missing value then
					if completed of staleReminder is false then
						set completed of staleReminder to true
						set completedCount to completedCount + 1
					end if
				end if
			end if
		end repeat
	end tell

	return "Created " & createdCount & ", updated " & updatedCount & ", completed stale " & completedCount & ", skipped duplicate rows " & skippedDuplicateCount & "."
end run
