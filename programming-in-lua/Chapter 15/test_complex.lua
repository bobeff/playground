function searchfile(name)
    local file1, error1 = package.searchpath(name, package.path)
    local file2, error2 = package.searchpath(name, package.cpath)

    if file1 then
        return loadfile(file1)
    elseif file2 then
        return package.loadlib(file2)
    else
        error(error1 .. "\n" .. error2, 2)
    end

end

package.searchers = { searchfile }

complex = require "complex"

local c1 = complex.new(1, 3)
local c2 = complex.new(2, 2)

print(complex.tostring(c1) .. " + " .. complex.tostring(c2) .. " = " ..
      complex.tostring(complex.add(c1, c2)))

print(complex.tostring(c1) .. " - " .. complex.tostring(c2) .. " = " ..
      complex.tostring(complex.sub(c1, c2)))

print(complex.tostring(c1) .. " * " .. complex.tostring(c2) .. " = " ..
      complex.tostring(complex.mul(c1, c2)))

print(complex.tostring(c1) .. " / " .. complex.tostring(c2) .. " = " ..
      complex.tostring(complex.div(c1, c2)))
