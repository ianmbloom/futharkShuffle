import "cpprandom"
import "hoas"

module dist = uniform_real_distribution f64 minstd_rand

type Rng = minstd_rand.rng
let mkRng (seed:i64) = minstd_rand.rng_from_seed [i32.i64 seed]

let uniformPick (rng:Rng) (len:i64):(Rng, i64) =
    let (rng0, fl) = dist.rand (0,f64.i64 len) rng
    in  (rng0, i64.f64 (f64.floor fl))

let swap_2d [h][w]
            (fromH:i64) (fromW:i64)
            (  toH:i64) (  toW:i64)
            (frame:*[h][w](i64,i64)):
            *[h][w](i64,i64) =
    let hold   = frame[  toH,  toW]
    let frame0 = frame  with [  toH,  toW] = frame[fromH,fromW]
    let frame1 = frame0 with [fromH,fromW] = hold
    in  frame1

let shuffleField_2d (h:i64) (w:i64)
                    (rng:Rng):
                    (Rng, *[h][w](i64,i64)) =
    let frameSize = h * w
    in  loop (rng0, output) = (rng, tabulate_2d h w (\row col -> (row,col)))
        for _i < frameSize
        do  let (rng1, fromH) = uniformPick rng0 (h - 1)
            let (rng2, fromW) = uniformPick rng1 (w - 1)
            let (rng3,   toH) = uniformPick rng2 (h - 1)
            let (rng4,   toW) = uniformPick rng3 (w - 1)
            in  (rng4, swap_2d fromH fromW toH toW output)

let access_2d 't [h][w]
              (frame:[h][w]t)
              (ih:i64,iw:i64):
              t =
    frame[ih,iw]

let shuffle_2d 't [h][w]
               (rng:Rng)
               (frame:[h][w]t):
               (Rng,  [h][w]t) =
    -- let (rng0, field) = shuffleField_2d h w rng
    let (rng0, field) = (rng, tabulate_2d h w (\row col -> ((h-1)-row,(w-1)-col)))
    let remap = map_2d (access_2d frame) field
    in  (rng0, remap)
