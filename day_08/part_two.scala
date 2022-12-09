import scala.io.Source

@main def run: Unit =
    var input = Source.fromFile("./day_08/input.txt").getLines().
        toArray.map(line => line.split(""))

    var result = 0
    
    for(i <- 0 to input.length - 1; j <- 0 to input(0).length - 1) {
        val curr = input(i)(j).toInt

        var top = i - 1
        var top_count = 0
        var top_max = -1
        while(top >= 0) {
            var iter = input(top)(j).toInt
            if(iter >= curr && iter > top_max) {
               top_count += 1 
            }
            if(iter < curr && iter >= top_max) {
                top_count += 1
            }
            top_max = iter.max(top_max)
            top -= 1
        }

        var right = j + 1
        var right_count = 0
        var right_max = -1
        while(right < input(0).length) {
            var iter = input(i)(right).toInt
            if(iter >= curr && iter > right_max) {
               right_count += 1 
            }
            if(iter < curr && iter >= right_max) {
                right_count += 1
            }
            right_max = iter.max(right_max)
            right += 1
        }

        var bottom = i + 1
        var bottom_count = 0
        var bottom_max = -1
        while(bottom < input.length) {
            var iter = input(bottom)(j).toInt
            if(iter >= curr && iter > bottom_max) {
               bottom_count += 1 
            }
            if(iter < curr && iter >= bottom_max) {
                bottom_count += 1
            }
            bottom_max = iter.max(bottom_max)
            bottom += 1
        }

        var left = j - 1
        var left_count = 0
        var left_max = -1
        while(left >= 0) {
            var iter = input(i)(left).toInt
            if(iter >= curr && iter > left_max) {
               left_count += 1 
            }
            if(iter < curr && iter >= left_max) {
                left_count += 1
            }
            left_max = iter.max(left_max)
            left -= 1
        }

        println((top_count, right_count, bottom_count, left_count))
        result = (top_count * right_count * bottom_count * left_count).max(result)
    }


    println(result)