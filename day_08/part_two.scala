import scala.io.Source

@main def run: Unit =
    var input = Source.fromFile("./day_08/input.txt").getLines().
        toArray.map(line => line.split(""))

    var result = input.map(line => line.map(_ => 0))
    
    for(i <- 0 to input.length - 1; j <- 0 to input(0).length - 1) {
        // To Top
        var t = i - 1
        var top_count = 0
        var top_max = "-1"
        while(t >= 0) {
           if(input(t)(j) > top_max) {
                top_max = input(t)(j)
                top_count += 1
           }
            t -= 1
        }
        // To Right
        var r = j + 1
        var right_count = 0
        var right_max = "-1"
        while(r < input(0).length) {
           if(input(i)(r) > right_max) {
                right_max = input(i)(r)
                right_count += 1
           }
            r += 1
        }
        // To Bottom
        var b = i + 1
        var bottom_count = 0
        var bottom_max = "-1"
        while(b < input.length) {
           if(input(b)(j) > bottom_max) {
                bottom_max = input(b)(j)
                bottom_count += 1
           }
            b += 1
        }
        // To Left
        var l = j - 1
        var left_count = 0
        var left_max = "-1"
        while(l >= 0) {
           if(input(i)(l) > left_max) {
                left_max = input(i)(l)
                left_count += 1
           }
            l -= 1
        }

        result(i)(j) = top_count * right_count * bottom_count * left_count
    }
    
    val max = result.map(x => x.max).max
    println(max)