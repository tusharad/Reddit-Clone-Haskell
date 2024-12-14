import React, {useEffect, useState} from "react";
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faThumbsUp as regularThumbsup, faThumbsDown as regularThumbsDown} from '@fortawesome/free-regular-svg-icons'

interface CommentProps {
    mainComment: {
        commentIDForCommentInfo: number;
        commentContentForCommentInfo: string;
        createdAtForCommentInfo: string;
        userNameForCommentInfo: string;
        commentUpvoteCount?: number;
        commentDownvoteCount?: number;
    };
    setCommentIdForReply: (commentId: number) => void;
    handleCommentVote: (isUpvote: boolean, commentId: number) => void;
    setIsModalOpen: (isModalOpen: boolean) => void;
}

const CommentCard: React.FC<CommentProps> = ({
    mainComment,
    setCommentIdForReply,
    handleCommentVote,
    setIsModalOpen
}) => {
    const [upvoteCount, setUpvoteCount] = useState(0);
    const [downvoteCount, setDownvoteCount] = useState(0);

    useEffect(() => {
        setUpvoteCount((mainComment.commentUpvoteCount) ? mainComment.commentUpvoteCount : 0);
        setDownvoteCount((mainComment.commentDownvoteCount) ? mainComment.commentDownvoteCount : 0);
    }, []);

    return (
        <div className="border p-4 mb-2 rounded shadow-sm bg-white card-bg shadow-lg rounded-lg mb-6 overflow-hidden">
            <div className="flex justify-between">
                <span className="font-semibold text-gray-500">{mainComment.userNameForCommentInfo}</span>
                <span className="text-sm text-gray-500">{new Date(mainComment.createdAtForCommentInfo).toLocaleDateString()}</span>
            </div>
            <p className="mt-2 text-gray-900">{mainComment.commentContentForCommentInfo}</p>
            <div className="flex mt-2 text-sm space-x-2">
                <button className="hover:text-blue-499" onClick={() => {
                    setCommentIdForReply(mainComment.commentIDForCommentInfo); setIsModalOpen(true)
                }} >
                    Reply</button>
                <button className="hover:text-green-500" onClick={() =>
                    handleCommentVote(true, mainComment.commentIDForCommentInfo)}><FontAwesomeIcon icon={regularThumbsup} />
                    {upvoteCount}</button>
                <button className="hover:text-red-500" onClick={() =>
                    handleCommentVote(false, mainComment.commentIDForCommentInfo)}><FontAwesomeIcon icon={regularThumbsDown} /> {downvoteCount}</button>
            </div>
        </div>
    )
}

export default CommentCard;


