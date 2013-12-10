sta1 = "www.station1.com"
sta2 = "www.station2.com"

stations =
{
    sta1 = "www.station1.com",
    sta2 = "www.station2.com",
}

function getstation(name)
    if name == "sta1" then
        return "www.station1.com"
    elseif name == "sta2" then
        return "www.station2.com"
    else
        error(string.format("Invalid station: %s", name))
    end
end
