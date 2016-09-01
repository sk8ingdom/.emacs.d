Set Outlook = CreateObject("Outlook.Application")
Set SelectedItem = Outlook.ActiveExplorer.Selection.Item(1)
Set Shell = CreateObject("Shell.Application")
ShellApp.ShellExecute "cmd", "/c echo [[outlook:" & SelectedItem.EntryID & "][" & Replace(SelectedItem.Subject, "&", "and") & "]] | clip", "", "runas", 1
