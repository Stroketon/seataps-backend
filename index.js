const Discord = require("discord.js")
let MongoDB = require("mongodb");
const express = require("express")
const crypto = require("crypto")
const fs = require("fs")
const app = express()
const evalAccess = ["666367959119298617"]
let Flags = {
    Trial: false
}
app.use(function(req, res, next) {
    const Headers = ["sentinel-fingerprint", "syn-fingerprint", "exploit-guid"]
    Headers.forEach(function(header) {
        if (req.headers[header]) {
            req.hwid = req.headers[header]
        }
    })
    next()
})
const Url = "mongodb://127.0.0.1:27017"
const prefix = "!"
let MongoClient = MongoDB.MongoClient
const Bot = new Discord.Client()
const Webhook = new Discord.WebhookClient("691617494766452807", "Pf_iyoa8YguykUiWdLk5nUPmaKS9LMjzDm2ekhCoNpHl32B88ibQSZ3p1TT7eGUHPnk5")
let Commands = {}
Bot.on("ready", function() {
    Bot.user.setActivity("Managing seataps whitelist")
})
let Filter = function(name) {
    const ValueName = name
    return function(ValName) {
        if (ValName === ValueName) {
            return false
        } else {
            return true
        }
    }
}
let GetCollection = function(cb) {
    MongoClient.connect(Url, (err, client) => {
        if (err) {throw err;}
        const db = client.db("Whitelist");
        const collection = db.collection("seataps");
        cb(collection, client);
    });
};
let GetScriptsCollection = function(cb) {
    MongoClient.connect(Url, (err, client) => {
        if (err) {throw err;}
        const db = client.db("Whitelist");
        const collection = db.collection("seatapsscript");
        cb(collection, client);
    });
};
let Embed = function(Des, fields) {
    const Emb = new Discord.MessageEmbed();
    Emb.setDescription(Des)
    Emb.setColor(0x3498DB)
    Emb.setFooter(Bot.user.username, Bot.user.displayAvatarURL())
    if (fields) {
        Emb.addFields(fields)
    }
    Emb.setTitle("Seataps Whitelister")
    Emb.setTimestamp()
    return Emb
}
let WebhookEmbed = function(Des, fields) {
    const Emb = new Discord.MessageEmbed();
    Emb.setDescription(Des)
    Emb.setColor(0x3498DB)
    Emb.setFooter("Seataps whitelist logs")
    if (fields) {
        Emb.addFields(fields)
    }
    Emb.setTitle("Seataps Whitelister")
    return Emb
}
String.prototype.replaceAll = function (find, replace) {
    var str = this.toString();
    return str.replace(new RegExp(find.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&'), 'g'), replace);
};

function CheckStr(Str) {
    const List = [" ", ""]
    List.forEach(function(value) {
        if (Str === value) {
            Str = "Empty Value"
        }
    })
    return Str
}
function SendLog(des, list) {
    const Fields = []
    for (const index in list) {
        Fields.push({name: index, value: CheckStr(list[index])})
    }
    const Embe = WebhookEmbed(des, Fields)
    Webhook.send("", {
        embeds: [Embe]
    })
}
function getKey(ID) {
    return new Promise(function(resolve, reject) {
        GetCollection(async function(collection, client) {
            let User = await collection.findOne({ID: ID})
            if (User) {
                resolve(`getgenv().Key = "${User.Key}"\n`)
            } else {
                resolve("")
            }
            client.close()
        })
    })
}
Bot.on("message", function(message) {
    const args = message.content.split(" ");
    const Sender = message.author.id
    const Mention = message.mentions.users.first()
    if (args[0].startsWith(prefix)) {
        const CommandName = args[0]
        if (Commands[CommandName]) {
            if (message.channel.type !== (Commands[CommandName].DmsOnly && "dm" || "text")) return
            if (Mention && Mention.bot) {
                message.channel.send(Embed("The user is a bot"))
                return
            }
            try {
                Commands[CommandName].callback(message, args.filter(Filter(CommandName)))
            }
            catch (err) {
                message.channel.send(Embed("There was an error in the command dm stroketon about it"))
                err.name = `${CommandName}`
                console.log(`${err.stack}`)
            }
        }
    }
})
function RandomString(length) {
    var result           = '';
    var characters       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    var charactersLength = characters.length;
    for ( var i = 0; i < length; i++ ) {
       result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
}
function addcommand(name, callback, DmsOnly) {
    Commands[prefix + name] = {callback: callback, DmsOnly: DmsOnly || false}
}
function GetServer() {
    return Bot.guilds.cache.array()[0]
}
function HMAC(Data, Key) {
    return crypto.createHash("sha256").update(Key + Data + Key).digest("hex")
}
function Sha256(str) {
    let Hash = crypto.createHash("sha256")
    Hash.update(str)
    return Hash.digest("hex").toUpperCase()
}
function randomBytes() {
    return crypto.randomBytes(20).toString("hex").toUpperCase()
}
function isUpperCase(Str) {
    return Str == Str.toUpperCase()
}
function hasNumber(Str) {
    let NumbersFound = 0
    Str.split("").forEach(function(Value) {
        if (parseInt(Value)) {
            NumbersFound++
        }
    })
    return NumbersFound >= 2
}
addcommand("whitelist", function(msg, args) {
    let Hwid = args[0]
    let Server = GetServer()
    if (Server.member(msg.author)) {
        if (!Hwid) {
            msg.channel.send(Embed("Please put a hwid"))
            return
        }
        let Member = Server.member(msg.author)
        let hasRole = Member.roles.cache.find(function(role) {
            return role.name == "Buyer"
        })
        if (hasRole) {
            let Key = RandomString(15)
            GetCollection(async function(collection, client) {
                if (Hwid.length === 64 && isUpperCase(Hwid) && hasNumber(Hwid)) {
                    let User = await collection.findOne({HWID: Hwid})
                    if (!User) {
                        collection.insertOne({HWID: Hwid, Key: Key, ID: msg.author.id})
                        msg.channel.send(Embed(`Your new key is\n\`\`\`${Key}\`\`\``))
                        SendLog("User has generated a key", {Hwid: Hwid, User: `<@${msg.author.id}>`, Key: Key})
                    } else {
                        msg.channel.send(Embed("Contact a whitelist changer inorder to rewhitelist"))
                    }
                } else {
                    msg.channel.send(Embed("Please put a vaild hwid"))
                }
                client.close()
            })
        }
    }
}, true)
addcommand("deleteuser", function(msg, args) {
    let Member = msg.guild.member(msg.author)
    let hasRole = Member.roles.cache.find(function(role) {
        return role.name == "Whitelister"
    })
    if (hasRole) {
        let Mention = msg.mentions.users.first()
        GetCollection(async function(collection, client) {
            if (Mention) {
                let User = await collection.findOne({ID: Mention.id})
                if (User) {
                    collection.deleteOne(User)
                    msg.channel.send(Embed("Successfully deleted the user"))
                } else {
                    msg.channel.send(Embed("User is not whitelisted"))
                }
            } else {
                msg.channel.send(Embed("Please add a mention"))
            }
            client.close()
        })
    } else {
        msg.channel.send(Embed("You do not have the perms to do that"))
    }
})
addcommand("setkey", function(msg, args) {
    let Server = GetServer()
    let Member = Server.member(msg.author)
    var hasRole = Member.roles.cache.find(function(role) {
        return role.name == "Buyer"
    })
    if (!hasRole) {
        msg.channel.send(Embed("You are not a buyer"))
        return
    }
    if (!args[0]) {
        msg.channel.send(Embed("Please put a key"))
        return
    }
    let Key = args[0]
    GetCollection(async function(collection, client) {
        let User = await collection.findOne({ID: msg.author.id})
        let KeyDoc = await collection.findOne({Key: Key})
        if (User) {
            if (!KeyDoc) {
                collection.updateOne(User, {$set: {Key: Key}})
                SendLog("User has changed their key", {User: `<@${msg.author.id}>`, From: User.Key, To: Key})
                msg.channel.send(Embed("Done"))
            } else {
                msg.channel.send(Embed("The key is already used"))
            }
        } else {
            msg.channel.send(Embed("Failed to find your DiscordID in the database"))
        }
        client.close()
    })
}, true)
function getDateFormat(timestamp) {
    let Months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    var Days = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
    let DateObject = new Date(timestamp)
    let Results = `${Days[DateObject.getDay()]}, ${DateObject.getDate()} ${Months[DateObject.getMonth()]} ${DateObject.getFullYear()}`
    return Results
}
addcommand("info", function(msg, args) {
    let Member = msg.guild.member(msg.author)
    let hasRole = Member.roles.cache.find(function(role) {
        return role.name == "Whitelister"
    })
    let Mention = msg.mentions.users.first()
    if (!Mention) return
    if (hasRole) {
        let Fields = []
        GetCollection(async function(collection, client) {
            let User = await collection.findOne({ID: Mention.id})
            if (User) {
                Object.keys(User).forEach(function(Index) {
                    if (Index === "_id") {
                        let Timestamp = MongoDB.ObjectID(User["_id"]).getTimestamp()
                        Fields.push({name: "Whitelist Date", value: getDateFormat(Timestamp)})
                    } else {
                        Fields.push({name: Index, value: User[Index]})
                    }
                })
                msg.author.send(Embed("Here is your info request", Fields))
                msg.channel.send(Embed("Check Dms"))
            } else {
                msg.channel.send(Embed("User was not found"))
            }
            client.close()
        })
    }
})
addcommand("ahere", function(msg, args) {
    let Member = msg.guild.member(msg.author)
    let Role = msg.guild.roles.cache.find(role => role.name == "Moderator")
    let Highest = Member.roles.highest
    if (Highest.comparePositionTo(Role) >= 0) {
        if (!args[0]) {
            msg.channel.send(Embed("Please add something"))
            return
        }
        let MentionChannel = msg.mentions.channels.first()
        if (!MentionChannel) {
            var Channel = msg.channel.guild.channels.cache.find(function(channel) {return channel.name == "announcements"})
        } else {
            var Channel = MentionChannel
            args.splice(0, 1)
        }
        let EmbedAnnouncement = Embed(args.join(" "))
        EmbedAnnouncement.setTitle("Announcement")
        Channel.send("@here", {embed: EmbedAnnouncement})
        msg.channel.send(Embed("Done"))
    } else {
        msg.channel.send(Embed("You do not have the perms to do that"))
    }
})
addcommand("aeveryone", function(msg, args) {
    let Member = msg.guild.member(msg.author)
    let Role = msg.guild.roles.cache.find(role => role.name == "Staff Manager")
    let Highest = Member.roles.highest
    if (Highest.comparePositionTo(Role) >= 0) {
        if (!args[0]) {
            msg.channel.send(Embed("Please add something"))
            return
        }
        let MentionChannel = msg.mentions.channels.first()
        if (!MentionChannel) {
            var Channel = msg.channel.guild.channels.cache.find(function(channel) {return channel.name == "announcements"})
        } else {
            var Channel = MentionChannel
            args.splice(0, 1)
        }
        let EmbedAnnouncement = Embed(args.join(" "))
        EmbedAnnouncement.setTitle("Announcement")
        Channel.send("@everyone", {embed: EmbedAnnouncement})
        msg.channel.send(Embed("Done"))
    } else {
        msg.channel.send(Embed("You do not have the perms to do that"))
    }
})
addcommand("anone", function(msg, args) {
    let Member = msg.guild.member(msg.author)
    let Role = msg.guild.roles.cache.find(role => role.name == "Support Team")
    let Highest = Member.roles.highest
    if (Highest.comparePositionTo(Role) >= 0) {
        if (!args[0]) {
            msg.channel.send(Embed("Please add something"))
            return
        }
        let MentionChannel = msg.mentions.channels.first()
        if (!MentionChannel) {
            var Channel = msg.channel.guild.channels.cache.find(function(channel) {return channel.name == "announcements"})
        } else {
            var Channel = MentionChannel
            args.splice(0, 1)
        }
        let EmbedAnnouncement = Embed(args.join(" "))
        EmbedAnnouncement.setTitle("Announcement")
        Channel.send(EmbedAnnouncement)
        msg.channel.send(Embed("Done"))
    } else {
        msg.channel.send(Embed("You do not have the perms to do that"))
    }
})
addcommand("getscript", function(msg, args) {
    let Server = GetServer()
    let Member = Server.member(msg.author)
    let hasRole = Member.roles.cache.find(function(role) {
        return role.name == "Buyer"
    })
    let ID = randomBytes()
    if (hasRole) {
        GetScriptsCollection(async function(collection, client) {
            collection.insertOne({ID: ID, DiscordID: msg.author.id})
            let Key = await getKey(msg.author.id)
            msg.author.send(Embed("Script loadstring", {name: "Script", value: `\`\`\`lua\n${Key}loadstring(game:HttpGet("http://seataps.com/getfile?ID=${ID}&Exploit=" .. (syn and "SynapseX")))()\`\`\``}))

            client.close()
        })
    }
}, true)
addcommand("eval", function(msg, args) {
    const Sender = msg.author.id
    if(!evalAccess.includes(Sender)) {return}
    try {
        const timebefore = process.hrtime()
        var Script = args.join(" ")
        if (Script.split("\n").length !== 1) {
            let Lines = Script.split("\n")
            if (Lines[0].search("```") == -1) {
                return
            }
            Lines.splice(0, 1)
            Lines.pop()
            Script = Lines.join("\n")
        }
        const evalresult = eval(Script)
        const timenow = process.hrtime(timebefore)[1] / 1000000
        msg.channel.send(Embed("Eval", [{name: "Done executing in " + timenow.toString() + " ms", value: "```" + require("util").inspect(evalresult) + "```", inline: false}]));
    }
    catch (err){
        msg.channel.send(Embed("Eval", [{name: "Error!", value:  "```" + err.message+ "```"}]))
    }
})
app.get("/Whitelist/GetHwid", function(req, res) {
    if (!req.hwid) {
        res.end("HWID was not found")
        return
    }
    let HWID = Sha256(req.hwid)
    res.end(HWID)
})
app.get("/Whitelist/Verify", function(req, res) {
    if (!req.hwid) {
        res.json({Success: false, Reason: "HWID was not found"})
        return
    }
    if (!req.query.Key) {
        res.json({Success: false, Reason: "Key was not found"})
        return
    }
    if (!req.query.Hash) {
        res.json({Success: false, Reason: "Hash was not found"})
        return
    }
    if (!req.headers.username) {
        res.json({Success: false, Reason: "Username header was not found"})
        return
    }
    let ServerHash = HMAC(req.query.Hash, "6bpYSERa8Ukn5HpS")
    let HWID = Sha256(req.hwid)
    GetCollection(async function(collection, client) {
        let User = await collection.findOne({Key: req.query.Key})
        if (User) {
            let Server = GetServer()
            let Member = Server.member(User.ID)
            if (Member) {
                var hasRole = Member.roles.cache.find(function(role) {
                    return role.name == "Buyer"
                })
            }
            if (User.HWID == HWID) {
                if (hasRole) {
                    res.json({Success: true, Hash: ServerHash})
                    SendLog("User has successfully logged in", {["Key Owner"]: `<@${User.ID}>`, Key: req.query.Key, HWID: HWID, ["Roblox username"]: req.headers.username})
                } else {
                    res.json({Success: false, Reason: "You are not allowed to seataps"})
                    SendLog("User failed to login, No role detected", {["Key Owner"]: `<@${User.ID}>`, Key: req.query.Key, HWID: HWID, ["Roblox username"]: req.headers.username})
                }
            } else {
                res.json({Success: false, Reason: "HWID Mismatch"})
                SendLog("User failed to login for HWID Mismatch", {["Key Owner"]: `<@${User.ID}>`, Key: req.query.Key, HWID: HWID, ["Roblox username"]: req.headers.username})
            }
        } else {
            if (Flags.Trial) {
                SendLog("User has successfully logged in", {Key: req.query.Key, HWID: HWID, ["Roblox username"]: req.headers.username})
                res.json({Success: true, Hash: ServerHash})
            } else {
                SendLog("User has failed to login with a invaild key", {Key: req.query.Key, HWID: HWID, ["Roblox username"]: req.headers.username})
                res.json({Success: false, Reason: "Invaild Key"})
            }
        }
        client.close()
    })
})
app.get("/getfile", function(req, res) {
    if (!req.query.ID) {
        res.end("error('ID was not found')")
        return
    }
    if (req.headers["user-agent"] !== "Roblox/WinInet") {
        res.end(`error("Invaild request")`)
        return
    }
    GetScriptsCollection(async function(collection, client) {
        let IDField = await collection.findOne({ID: req.query.ID})
        if (IDField) {
            let Server = GetServer()
            let Member = Server.member(IDField.DiscordID)
            if (Member) {
                var hasRole = Member.roles.cache.find(function(role) {
                    return role.name == "Buyer"
                })
            }
            if (hasRole) {
                let Results = `error("The exploit does not exist")`
                fs.readdirSync("./Scripts").forEach(function(file) {
                    if (file == req.query.Exploit + ".lua") {
                        Results = fs.readFileSync(`./Scripts/${file}`)
                    }
                })
                res.end(Results)
            } else {
                res.end(`error("You are not buyer")`)
            }
        } else {
            if (Flags.Trial) {
                let Results = `error("The exploit does not exist")`
                fs.readdirSync("./Scripts").forEach(function(file) {
                    if (file == req.query.Exploit + ".lua") {
                        Results = fs.readFileSync(`./Scripts/${file}`)
                    }
                })
                res.end(Results)
            } else {
                res.end(`error("Invaild ID")`)
            }
        }
        client.close()
    }) 
}) 
app.use("/files", express.static("./files"))
app.listen(80)
Bot.login("")
SendLog("Bot is ready")