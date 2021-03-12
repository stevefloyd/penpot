;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.ui.workspace.shapes.frame
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.main.data.workspace :as dw]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.context :as muc]
   [app.main.ui.shapes.frame :as frame]
   [app.main.ui.shapes.shape :refer [shape-container]]
   [app.util.dom :as dom]
   [app.util.keyboard :as kbd]
   [app.util.timers :as ts]
   [beicon.core :as rx]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

#_(defn use-select-shape [{:keys [id]} edition]
  (mf/use-callback
   (mf/deps id edition)
   (fn [event]
     (when (not edition)
       (let [selected @refs/selected-shapes
             selected? (contains? selected id)
             shift? (kbd/shift? event)]
         (cond
           (and selected? shift?)
           (st/emit! (dw/select-shape id true))

           (and (not (empty? selected)) (not shift?))
           (st/emit! (dw/deselect-all) (dw/select-shape id))

           (not selected?)
           (st/emit! (dw/select-shape id))))))))

;; Ensure that the label has always the same font
;; size, regardless of zoom
;; https://css-tricks.com/transforms-on-svg-elements/
(defn text-transform
  [{:keys [x y]} zoom]
  (let [inv-zoom (/ 1 zoom)]
    (str
     "scale(" inv-zoom ", " inv-zoom ") "
     "translate(" (* zoom x) ", " (* zoom y) ")")))

(mf/defc frame-title
  [{:keys [frame]}]
  (let [{:keys [width x y]} frame
        zoom                 (mf/deref refs/selected-zoom)
        edition              (mf/deref refs/selected-edition)
        label-pos            (gpt/point x (- y (/ 10 zoom)))
        ;;handle-click         (use-select-shape frame edition)
        ]
    #_[:text {:x 0
            :y 0
            :width width
            :height 20
            :class "workspace-frame-label"
            :transform (text-transform label-pos zoom)
            :on-click handle-click}
     (:name frame)]))

(defn make-is-moving-ref
  [id]
  (let [check-moving (fn [local]
                       (and (= :move (:transform local))
                            (contains? (:selected local) id)))]
    (l/derived check-moving refs/workspace-local)))

;; This custom deffered don't deffer rendering when ghost rendering is
;; used.
(defn custom-deferred
  [component]
  (mf/fnc deferred
    {::mf/wrap-props false}
    [props]
    (let [ghost? (mf/use-ctx muc/ghost-ctx)
          tmp (mf/useState false)
          ^boolean render? (aget tmp 0)
          ^js set-render (aget tmp 1)]
      (mf/use-layout-effect
       (fn []
         (let [sem (ts/schedule-on-idle #(set-render true))]
           #(rx/dispose! sem))))
      (if ghost?
        (mf/create-element component props)
        (when render? (mf/create-element component props))))))

(defn frame-wrapper-factory
  [shape-wrapper]
  (let [frame-shape (frame/frame-shape shape-wrapper)]
    (mf/fnc frame-wrapper
      {::mf/wrap [#(mf/memo' % (mf/check-props ["shape" "objects"])) custom-deferred]
       ::mf/wrap-props false}
      [props]
      (let [shape   (unchecked-get props "shape")
            objects (unchecked-get props "objects")
            ghost? (mf/use-ctx muc/ghost-ctx)
            edition       (mf/deref refs/selected-edition)

            shape         (gsh/transform-shape shape)
            children      (mapv #(get objects %) (:shapes shape))
            ds-modifier   (get-in shape [:modifiers :displacement])]

        (when (and shape (not (:hidden shape)))
          [:g.frame-wrapper 
           [:& frame-title {:frame shape}]

           [:> shape-container {:shape shape}
            [:& frame-shape
             {:shape shape
              :childs children}]]])))))

