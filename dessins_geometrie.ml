(*********************************************************************)
(* TYPES DE BASE *)

type color = Graphics.color

type mode = FILLED | OUTLINED

type box = {width:int; height:int}

let box w h = {width=w; height=h}


(*********************************************************************)
(* LE TYPE IMG *)

type img =
  | Ellipse of int * int * color * mode
  | Line of int * int * int * int * color
  | Polygon of (int * int) array * color * mode
  | Underlay_at of img * int * int * img
             
                        
(*********************************************************************)
(* LES FONCTIONS DE CALCUL*)

(*__________polygon_________*)
(* calcul le (min,max) d'un tableau pour la box*)
let max_min_x tab =
  let ma = ref (fst tab.(0)) in
  let mi = ref (fst tab.(0)) in
  for i = 1 to (Array.length tab-1) do
    if !ma < fst tab.(i) then ma := fst tab.(i)
    else if !mi > fst tab.(i) then mi := fst tab.(i)
    else ()
  done;
  (!ma, !mi)

let max_min_y tab =
  let ma = ref (snd tab.(0)) in
  let mi = ref (snd tab.(0)) in
  for i = 1 to (Array.length tab-1) do
    if !ma < snd tab.(i) then ma := snd tab.(i)
    else if !mi > snd tab.(i) then mi := snd tab.(i)
    else ()
  done;
  (!ma, !mi)

(*décale le polygone avec les min du tableau et la position initiale x y*)
let decale_tableau tab x y =
  let min_x = snd (max_min_x tab) in
  let min_y = snd (max_min_y tab) in
  for i = 0 to (Array.length tab-1) do
    tab.(i) <- ((fst tab.(i)) - min_x + x, (snd tab.(i)) - min_y + y) 
  done;
  tab

(*____________regular_polygon_______________*)
(*tableau de points*)
let pi = 3.14159265
let theta n = 2.*.pi /. (float_of_int n)
let phi n = (pi +. (theta  n))/.2.
let point_z k n c =
  let modulus = (float_of_int c) /. (2.*.(sin ((theta n)/.2.))) in
  (int_of_float (modulus *. (cos ((float_of_int k) *. (theta n) -. (phi n)))),
   int_of_float (modulus *. (sin ((float_of_int k) *. (theta n) -. (phi n)))))

let points_regular_polygon n c =
  Array.init n (fun k -> (point_z k n c))

  
(*********************************************************************)
(* LES FONCTIONS DE CALCUL ET DESSIN *)
              
let rec box_of_img = function
  | Ellipse(w, h, _, _) -> box (2*w) (2*h)
  | Line(x1, y1, x2, y2, _) -> box (abs(x2 - x1)) (abs(y2 - y1))
  | Polygon(t, _, _)
    -> box (abs(fst(max_min_x t) - snd(max_min_x t)))
           (abs(fst(max_min_y t) - snd(max_min_y t)))
  | Underlay_at(img1, dx, dy, img2)
    -> box (max ((box_of_img img1).width + (abs dx - dx)/2) ((box_of_img img2).width + (abs dx + dx)/2))
         (max ((box_of_img img1).height + (abs dy - dy)/2) ((box_of_img img2).height + (abs dy + dy)/2))

let rec draw_at (x,y) = function
  | Ellipse(w, h, color, FILLED) -> begin
      Graphics.set_color color;
      Graphics.fill_ellipse (x+w) (y+h) w h
    end
  | Ellipse(w, h, color, OUTLINED) -> begin
      Graphics.set_color color;
      Graphics.draw_ellipse (x+w) (y+h) w h
    end
  | Line(x1, y1, x2, y2, color) -> begin
      Graphics.set_color color;
      Graphics.moveto (x + (abs(x2-x1) - (x2-x1))/2) (x + (abs(y2-y1) - (y2-y1))/2);
      Graphics.lineto (x + (abs(x2-x1) + (x2-x1))/2) (y + (abs(y2-y1) + (y2-y1))/2)
      (*On décale la ligne*)
    end
  | Polygon(tab, color, FILLED) -> begin
      Graphics.set_color color;
      Graphics.fill_poly (decale_tableau tab x y)
    end
  | Polygon(tab, color, OUTLINED) -> begin
      Graphics.set_color color;
      Graphics.draw_poly (decale_tableau tab x y)
    end
   | Underlay_at(img1, dx, dy, img2) -> begin
      draw_at (x + (abs dx - dx)/2, y + (abs dy - dy)/2) img1;
      draw_at (x + (abs dx + dx)/2, y + (abs dy + dy)/2) img2
    end

(*********************************************************************)
(* LES FONCTIONS RACCOURCIS DES CONSTRUCTEURS *)

let rectangle w h color mode = Polygon([|(0,0); (0,h); (w,h); (w,0)|], color, mode)

let square n color mode = Polygon([|(0,0); (0,n); (n,n); (n,0)|], color, mode)
    
let ellipse w h color mode = Ellipse(w, h, color, mode)
    
let circle radius color mode = Ellipse(radius, radius, color, mode)

let line (x1,y1) (x2, y2) color = Line(x1, y1, x2, y2, color)

let polygon tab color mode = Polygon(tab, color, mode)

let rhombus w h color mode = Polygon([|(0,h/2); (w/2,0); (w,h/2); (w/2,h)|], color, mode)

let regular_polygon n c color mode = Polygon((points_regular_polygon n c), color, mode)
(*faut-t-il faire le cas n <= 0 ?*)
let beside img1 img2 = Underlay_at (img1, (box_of_img img1).width , 0, img2)
let beside_center img1 img2 =
  Underlay_at (img1, (box_of_img img1).width ,
               ((box_of_img img1).height - (box_of_img img2).height)/2, img2)
let beside_top img1 img2 =
  Underlay_at (img1, (box_of_img img1).width ,
     (box_of_img img1).height - (box_of_img img2).height, img2)
let empty_img = circle 0 Graphics.red OUTLINED
let beside_list list_img = match list_img with
  | [] -> empty_img
  | im_hd :: im_tl -> List.fold_left beside im_hd im_tl

let above img1 img2 =
  Underlay_at (img1, 0, (box_of_img img1).height, img2)
let above_right img1 img2 =
  Underlay_at (img1, (box_of_img img1).width - (box_of_img img2).width,
               (box_of_img img1).height, img2)
let above_center img1 img2 =
  Underlay_at (img1,((box_of_img img1).width - (box_of_img img2).width)/2,
               (box_of_img img1).height ,img2)

let underlay_at img1 (dx,dy) img2 = Underlay_at (img1, dx, dy, img2)
let underlay img1 img2 =
  Underlay_at (img1, ((box_of_img img1).width - (box_of_img img2).width)/2,
               ((box_of_img img1).height - (box_of_img img2).height)/2 , img2)
let underlay_center_top img1 img2 =
  Underlay_at (img1, ((box_of_img img1).width - (box_of_img img2).width)/2,
               (box_of_img img1).height - (box_of_img img2).height, img2)
let underlay_center_bot img1 img2 =
  Underlay_at (img1, ((box_of_img img1).width - (box_of_img img2).width)/2,
               0, img2)
let underlay_center_left img1 img2 =
  Underlay_at (img1, 0,
               ((box_of_img img1).height - (box_of_img img2).height)/2, img2)  
let underlay_center_right img1 img2 =
  Underlay_at (img1, (box_of_img img1).width - (box_of_img img2).width,
              ((box_of_img img1).height - (box_of_img img2).height)/2 , img2)

let framed img1 = underlay img1 (rectangle (box_of_img img1).width (box_of_img img1).height Graphics.black OUTLINED)

let last_size = ref 0
  
let with_pen_size n img1 =
  Graphics.set_line_width !last_size;
  last_size := n;
  let img2 = Graphics.set_line_width n; img1 in
  img2
  
  
let above_center2 img1 img2 = (*superpose 1 cercle sur 2 cercle*)
  Underlay_at (img1, ((box_of_img img1).width - (box_of_img img2).width)/2,
               7*(box_of_img img1).height/8 ,img2)

  
(*********************************************************************)
(* LA FONCTIONS DISPLAY *)


let display img =
  let {width;height} = box_of_img img in
  Graphics.resize_window width height;
  draw_at (0, 0) img;
  Graphics.loop_at_exit [Key_pressed] (fun _ -> raise Exit)

(*********************************************************************)
(* LA FONCTION MAIN *)

let main () =
  Graphics.open_graph "" ;

  let my_cool_image =

    (*let (||) = beside in
    let cb = circle 100 Graphics.black OUTLINED in
    with_pen_size 5 cb*)

    (*------------------choix aleat couleurs-------------*)

    let tomate = [|0xa50f15; 0xde2d26; 0xfb6a4a; 0xfc9272; 0xfcbba1; 0xfee5d9|] in
    let poisson = [|0x08519c; 0x3182bd; 0x6baed6; 0x9ecae1; 0xc6dbef; 0xeff3ff|] in
    let epinards = [|0x006837;0x31a354;0x78c679;0xaddd8e;0xd9f0a3;0xffffcc|] in
    let betterave = [|0x7a0177;0xc51b8a;0xf768a1;0xfa9fb5;0xfcc5c0;0xfeebe2|] in
    let steak = [|0xbd0026;0xf03b20;0xfd8d3c; 0xfeb24c; 0xfed976;0xffffb2|] in
                                  
    let menu = [|tomate; poisson; epinards; betterave; steak|].(Random.int 5) in

    (*-------------------pink_flamingo----------------------*)
    let gris_pattes = 0x5c5859 in
    let pink = 0xd55390 in
    let pale = 0xdd8a9e in
    let noir = 0x3a0f19 in
    let gris_bec = 0x5a5956 in
    
    let pol1 = polygon [|(141,0);(258,0);(206,31);(206,144);(340,144);(232,305);(214,305);(311,156);(206,156);(206,305);(190,305);(190,156);(92,156);(133,72);(133,144);(190,144);(190,31)|] gris_pattes FILLED in
    let pol6 = polygon [|(0,525);(102,452);(290,648);(290,571);(402,594);(552,453);(342,452);(477,194);(282,305);(123,305)|] pink FILLED in
    let pol7 = polygon [|(460,206);(149,380);(0,525);(124,769);(20,769);(81,868);(164,769);(50,525);(102,452)|] pale FILLED in
    let pol9 = polygon [|(434,275);(342,452);(191,451);(403,700);(403,454);(523,452)|] pale FILLED in
    let pol10 = polygon [|(83,769);(81,868);(164,769)|] pink FILLED in
    let cir11 = circle 9 noir FILLED in 
    let pol12 = polygon [|(35,728);(18,769);(79,769)|] gris_bec FILLED in
    let pol13 = polygon [|(33,725);(66,754);(66,704);|] noir FILLED in
    
    let pink_flamingo = underlay_at (underlay_at (underlay_at (underlay_at (underlay_at (underlay_at (underlay_at pol1 (-92,194) pol6) (0,205) pol7) (189,275) pol9) (80, 769) pol10) (45,785) cir11) (21, 727) pol12) (38, 708) pol13 in

    (*----------------------fractal-------------------------*)
    let p = 1./.3. in
    let rec fractal n c = match n with
      |0 -> underlay (circle c menu.(1) FILLED) (circle c menu.(0) OUTLINED) (*mettre largeur trait = 5*)
      |n -> let im = fractal (n-1) (int_of_float(p*.(float_of_int c))) in
        underlay
          (underlay
             (circle c menu.(n+1) FILLED)
             (with_pen_size 2 (circle c menu.(n) OUTLINED)))
          (above_center2
             (beside im im)               (* OO  *)
             (above_center2
                (beside(beside im im) im) (* OOO *) 
                (beside im im)))          (* OO  *)
    in  underlay (square 950 menu.(5) FILLED) (beside_center (fractal 2 200) pink_flamingo)
  (*fractal 3 200 est plus jolie mais prend 10 secondes à s'afficher*)
  in display my_cool_image

let () = main ()
    
