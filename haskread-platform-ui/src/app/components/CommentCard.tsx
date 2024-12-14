import React, {useEffect, useState} from "react";
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';
import {faThumbsUp as regularThumbsUp, faThumbsDown as regularThumbsDown} from '@fortawesome/free-regular-svg-icons';
import {faThumbsUp as solidThumbsUp, faThumbsDown as solidThumbsDown} from '@fortawesome/free-solid-svg-icons';

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
    setIsModalOpen: (isModalOpen: boolean) => void;
}

interface CurrUserCommentVotes {
    commentIDForFetchVote: number;
    isUpvote: boolean;
}

const CommentCard: React.FC<CommentProps> = ({
    mainComment,
    setCommentIdForReply,
    setIsModalOpen,
}) => {
    const [upvoteCount, setUpvoteCount] = useState(0);
    const [downvoteCount, setDownvoteCount] = useState(0);
    const [upvoteIcon, setUpvoteIcon] = useState(regularThumbsUp);
    const [downvoteIcon, setDownvoteIcon] = useState(regularThumbsDown);

    const getCommentsReaction = async (cId: number): Promise<CurrUserCommentVotes[]> => {
        const token = localStorage.getItem('jwt_token');
        if (token) {
            console.log("got token2", token)
            try {
                const response = await fetch('http://localhost:8085/api/v1/user/comment_votes', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `Bearer ${token}`,
                    },
                    body: JSON.stringify([cId]),
                });

                if (response.ok) {
                    const data = await response.json();
                    return data;
                } else {
                    const res = await response.text();
                    console.log(res, 'Failed to create thread');
                    return [];
                }
            } catch (error) {
                console.log('An error occurred: ' + error);
                return [];
            }
        } else {
            console.log("could not find token")
            return [];
        }
    }

    const updateLocalVoteStates = (upvote: boolean) => {
        if (upvote) {
            if (upvoteIcon === solidThumbsUp) {
                console.log("removing upvote");
                setUpvoteCount(upvoteCount - 1); setUpvoteIcon(regularThumbsUp);
            } else {
                if (downvoteIcon == solidThumbsDown) {
                    console.log("removing downvote");
                    setDownvoteCount(downvoteCount - 1); setDownvoteIcon(regularThumbsDown);
                }
                setUpvoteCount(upvoteCount + 1); setUpvoteIcon(solidThumbsUp);
            }
        } else {
            if (downvoteIcon == solidThumbsDown) {
                console.log("removing downvote");
                setDownvoteCount(downvoteCount - 1); setDownvoteIcon(regularThumbsDown);
            } else {
                console.log("downvote is happening");
                if (upvoteIcon == solidThumbsUp) {
                    setUpvoteCount(upvoteCount - 1); setUpvoteIcon(regularThumbsUp);
                }
                setDownvoteCount(downvoteCount + 1); setDownvoteIcon(solidThumbsDown);
            }
        }
    }

    const handleCommentVote = async (upvote: boolean, commentId: number) => {
        const token = localStorage.getItem('jwt_token');
        if (!token) {
            console.log('User is not authenticated');
            return;
        }
        try {
            const response = await fetch(`http://localhost:8085/api/v1/user/comment/vote/${commentId}/${upvote}`, {
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/json',
                    Authorization: `Bearer ${token}`,
                },
            });
            if (response.ok)
                updateLocalVoteStates(upvote);
            else
                console.log(await response.text());
        } catch (error) {
            console.log('An error occurred: ' + error);
        }
    };

    useEffect(() => {
        const fetchData = async (cId: number) => {
            const res = await getCommentsReaction(mainComment.commentIDForCommentInfo);
            setUpvoteCount(mainComment.commentUpvoteCount ? mainComment.commentUpvoteCount : 0);
            setDownvoteCount(mainComment.commentDownvoteCount ? mainComment.commentDownvoteCount : 0);
            for (let i = 0; i < res.length; i++) {
                if (cId == res[i].commentIDForFetchVote) {
                    if (res[i].isUpvote)
                        setUpvoteIcon(solidThumbsUp);
                    else
                        setDownvoteIcon(solidThumbsDown);
                }
            }
        }
        fetchData(mainComment.commentIDForCommentInfo);
    }, []);

    return (
        <div className="border p-4 mb-2 rounded shadow-sm bg-white card-bg shadow-lg rounded-lg mb-6 overflow-hidden">
            <div className="flex justify-between">
                <span className="font-semibold text-gray-500">{mainComment.userNameForCommentInfo} </span>
                <span className="text-sm text-gray-500">{new Date(mainComment.createdAtForCommentInfo).toLocaleDateString()}</span>
            </div>
            <p className="mt-2 text-gray-900">{mainComment.commentContentForCommentInfo}</p>
            <div className="flex mt-2 text-sm space-x-2">
                <button className="hover:text-blue-499" onClick={() => {
                    setCommentIdForReply(mainComment.commentIDForCommentInfo);
                    setIsModalOpen(true);
                }}>
                    Reply
                </button>
                <button className="hover:text-green-500" onClick={() =>
                    handleCommentVote(true, mainComment.commentIDForCommentInfo)}>
                    <FontAwesomeIcon icon={upvoteIcon} />
                    {upvoteCount}
                </button>
                <button className="hover:text-red-500" onClick={() =>
                    handleCommentVote(false, mainComment.commentIDForCommentInfo)}>
                    <FontAwesomeIcon icon={downvoteIcon} />
                    {downvoteCount}
                </button>
            </div>
        </div>
    );
}

export default CommentCard;
