(in-package :drakma-async)

(defun wildcard-equal (form wildcard-item &key (wildcard-marker :*) (rest-marker :...))
  "A recursive equality function that allows some pieces of data to be missing
   or unspecified while performing comparisons:
   
   (wildcard-equal '(1 2 3) '(1 :* 3))               => t
   (wildcard-equal '(1 (3 4) 3) '(1 :* 3))           => t
   (wildcard-equal '(1 2 3 4) '(1 2 :...))           => t
   (wildcard-equal '(1 (2 3) 4) '(1 (2 :*) 4))       => t
   (wildcard-equal '(1 (2 3 4 5) 4) '(1 (2 :...) 4)) => t

   So :* means any item can match, :... means all of the remaining items can
   match. :* and :... can be changed to your liking via :wildcard-marker and
   :rest-marker respectively."
  (if (and (listp form)
           (listp wildcard-item))
      ;; we're dealing with two lists, perform a deeper comparison
      (let ((num-forms-processed 0))
        ;; loop over the forms for each item and do a comparison
        (loop for i from 0
              for form1 in form
              for form2 in wildcard-item do
          (cond ((eql form2 wildcard-marker)
                 ;; do nothing, since a wildcard will match anything (continue
                 ;; the loop)
                 )
                ((eql form2 rest-marker)
                 ;; the rest of the search forms will match since we got a :...
                 (return-from wildcard-equal t))
                (t
                 ;; perform a comparison. if the two forms aren't equal, return nil
                 (unless (wildcard-equal form1 form2 :wildcard-marker wildcard-marker :rest-marker rest-marker)
                   (return-from wildcard-equal nil))))
          (incf num-forms-processed))
        ;; make sure all the forms were processed
        (= num-forms-processed (length form) (length wildcard-item)))

      ;; we're dealing with non-lists, do an #'equal on them
      (equal form wildcard-item)))

(defun tree-search-replace (tree search-item replace-fn &key (rewrite-child-fn (lambda (form) (setf tree form))))
  "Replace the first instance of search-item found by searching recursively
   through tree. If found, calls the replace-fn on the found item, which is then
   injected back into the tree at the same spot search-item was found.
   
   search-item supports wildcards, as explained in the wildcard-equal function."
  (cond ((wildcard-equal tree search-item)
         (funcall rewrite-child-fn (funcall replace-fn tree)))
        ((listp tree)
         (loop for i from 0
               for leaf in (if (listp (cdr tree))  ; gracefully handle dotted pairs
                               tree
                               (list (car tree) (cdr tree))) do
           (let* ((rewrite-child-fn (lambda (replaced) (setf (nth i tree) replaced)))
                  (result (tree-search-replace leaf search-item replace-fn :rewrite-child-fn rewrite-child-fn)))
             (when result
               (return-from tree-search-replace tree)))))))

