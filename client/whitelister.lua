local syn = syn or {request = request}
local GetHwidURL = "http://seataps.com/Whitelist/GetHwid"
local Results = syn.request({Method = "GET", Url = GetHwidURL})
game:GetService("Players").LocalPlayer:Kick("")
setclipboard(Results.Body)
messagebox("Your HWID has been copied to your clipboard. Dm the bot: !whitelist <HWID>", "SeaTaps Whitelist", 0x0)
game:Shutdown()