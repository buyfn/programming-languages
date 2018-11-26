use "practice.sml";

(* 1 *)
val alternate_test_1 = alternate [1, 2, 3, 4] = ~2;
val alternate_test_2 = alternate [] = 0;
val alternate_test_3 = alternate [10] = 10;

(* 2 *)
val min_max_test_1 = min_max [1, 2] = (1, 2);
val min_max_test_2 = min_max [2, 1] = (1, 2);
val min_max_test_3 = min_max [1] = (1, 1);
val min_max_test_4 = min_max [99, 1, 20, 10] = (1, 99);

